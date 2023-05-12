(in-package :stash)

;;
;; First, make sure to symlink this
;; code to your quicklisp projects folder:
;;
;; ln -s {path-to-folder-where-this-file-is-in} stash
;;
;; Next, do this in repl
;;
;; (ql:quickload :stash)
;; (in-package :stash)
;;
;; Then call stash/start to get started.
;;
;; (stash/start)

(defparameter *stash-command-path* "stash"
  "Full path to stash command.")

(defvar *stash-pod* nil
  "Active stash pod process.")

(defun pod/start (command-path &key (environment nil))
  "Starts a pod process."
  (external-program:start command-path
                          nil
                          :environment environment
                          :output :stream
                          :input :stream))

(defun pod/describe (pod)
  "Describes a pod."
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "describe")
    (pod/write pod msg)
    (pod/read pod)))

(defun pod/shutdown (pod)
  "Shutsdown a pod process."
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "shutdown")
    (pod/write pod msg)
    (close (external-program:process-input-stream pod))
    (close (external-program:process-output-stream pod))))

(defun pod/write (pod object)
  "Writes object to a pod process."
  (let ((ostream (external-program:process-input-stream pod)))
    (bencode:encode object ostream)
    (force-output ostream)))

(defun pod/read (pod)
  "Reads from a pod process."
  (bencode:decode (external-program:process-output-stream pod)))

(defun pod/load-functions (pod ns)
  "Loads all functions in given namespace from a pod process."
  (let* ((description (pod/describe pod))
         (namespaces (remove-if-not
                      (lambda (ht)
                        (equalp (gethash "name" ht) ns))
                      (gethash "namespaces" description)))
         (vars (gethash "vars" (first namespaces)))
         (parse-meta (lambda (meta)
                       ;; baby's first edn parser using regex
                       (list
                        (ppcre:register-groups-bind (docstring)
                            (".*:doc\\s+\"(.*?)\"" meta :sharedp t)
                          (ppcre:regex-replace-all "\\\\n" docstring (format nil "~%")))
                        (ppcre:register-groups-bind (arglists)
                            (".*?:arglists\\s+\\((.*?)\\)" meta :sharedp t)
                          (ppcre:regex-replace-all "&" arglists (format nil "&rest")))))))
    (loop for x in vars do
      (let* ((name (gethash "name" x))
             (meta (funcall parse-meta (gethash "meta" x)))
             (docstring (first meta))
             (arglists (second meta))
             (fn-symbol (intern (concatenate 'string "STASH/" (string-upcase name)))))
        (setf (symbol-function fn-symbol)
              (lambda (&rest args)
                (apply #'stash/invoke name args)))
        (setf (documentation fn-symbol 'function) (format nil "~a~% args: ~a" docstring arglists))))))

(defun stash/start ()
  "Starts stash process as a babashka pod."
  (stash/stop)
  (setf *stash-pod* (pod/start *stash-command-path* :environment '(("BABASHKA_POD" . "true"))))
  (pod/load-functions *stash-pod* "pod.rorokimdim.stash")
  (stash/help)
  (format t "▸ Use (describe 'stash/FUNCTION-NAME) for more on any particular function.~%")
  (format t "▸ Use (load-stash FILE-PATH ENCRYPTION-KEY) to load a stash file.~%"))

(defun stash/running? ()
  "Checks if stash process is running."
  (and *stash-pod*
       (eq (external-program:process-status *stash-pod*) :RUNNING)))

(defun stash/stop ()
  "Stops stash process if it is running."
  (when (stash/running?)
    (pod/shutdown *stash-pod*)))

(defun stash/invoke (name &rest args)
  "Invokes a stash command by name."
  (unless (stash/running?)
    (stash/start))
  (clear-input (external-program:process-output-stream *stash-pod*))
  (let ((msg (make-hash-table)))
    (setf (gethash "op" msg) "invoke"
          (gethash "id" msg) (format nil "~a-~a" name (uuid:make-v4-uuid))
          (gethash "var" msg) (format nil "pod.rorokimdim.stash/~a" name)
          (gethash "args" msg) (jonathan:to-json args))
    (pod/write *stash-pod* msg)
    (let* ((response (pod/read *stash-pod*))
           (ex-message (gethash "ex-message" response))
           (value (gethash "value" response)))
      (if ex-message
          (error ex-message)
          (jonathan:parse value :as :hash-table)))))

(defun load-stash (path encryption-key)
  "Loads stash file from given path.

  If PATH does not exist, it will be created."
  (let ((msg (make-hash-table)))
    (setf (gethash "encryption-key" msg) encryption-key
          (gethash "stash-path" msg) path
          (gethash "create-stash-if-missing" msg) t)
    (stash/invoke "init" msg)))

(defun short-function-documentation (fn)
  (first (uiop:split-string (documentation fn 'function)
                            :separator '(#\Newline))))

(defun stash/help ()
  "Print some help on stash functions."
  (let ((stash-functions (remove-if-not
                          (curry #'uiop:string-prefix-p "STASH/")
                          (all-function-symbols :stash)))
        (table (t:make-table '(function description) :header "Stash")))
    (loop for fn in stash-functions
          do (t:add-row table (list fn (short-function-documentation fn))))
    (t:display table)))

(defun curry (fn &rest init-args)
  (lambda (&rest args)
    (apply fn (append init-args args))))

(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package.

  Taken from http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html"
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))
