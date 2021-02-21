(in-package :stash)

;;
;; Do this in repl
;;
;; (ql:quickload :stash)
;; (in-package :stash)

(defparameter *stash-command-path* "stash"
  "Full path to stash command.")

(defvar *stash-path* "sample.stash"
  "Path to stash file.")

(defvar *stash-encryption-key* "sample-ekey"
  "Encryption key to use.")

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

(defun stash/start ()
  "Starts stash process as a babashka pod."
  (setf *stash-pod* (pod/start *stash-command-path* :environment '(("BABASHKA_POD" . "true")))))

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

(defun stash/init ()
  "Initializes stash.

  The encryption key is read from STASH_ENCRYPTION_KEY environment variable.

  If `STASH_FILE_PATH` does not exist, it will be created."
  (let ((msg (make-hash-table)))
    (setf (gethash "encryption-key" msg) *stash-encryption-key*
          (gethash "stash-path" msg) *stash-path*
          (gethash "create-stash-if-missing" msg) t)
    (stash/invoke "init" msg)))

(defun stash/version ()
  "Gets version of stash command."
  (stash/invoke "version"))

(defun stash/nodes (&optional (parent-id 0))
  "Gets all nodes stored in stash.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  (stash/invoke "nodes" parent-id))

(defun stash/tree (&optional (parent-id 0))
  "Gets all nodes stored in stash as a tree.

  Returns a hash-table of the form {key: [node-id, value, child-tree]}.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  (stash/invoke "tree" parent-id))

(defun stash/tree->tree-on-id (stree)
  "Converts a stash-tree to an equivalent tree indexed on node-ids."
  (let ((ht (make-hash-table)))
    (loop for k being the hash-keys in stree
            using (hash-value (node-id node-value child-stree))
          do
             (let ((m (make-hash-table)))
               (setf (gethash :key m) k
                     (gethash :value m) node-value
                     (gethash :children m) (if (not child-stree)
                                               (make-hash-table)
                                               (stash/tree->tree-on-id child-stree)))
               (setf (gethash node-id ht) m)))
    ht))

(defun stash/tree-on-id (&optional (parent-id 0))
  "Gets all nodes stored in stash as a tree indexed by node-ids.

  Returns a hash-map of the form {id {:key key :value value :children child-tree}}.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  (stash/tree->tree-on-id (stash/tree parent-id)))

(defun stash/tree-on-id->paths (tree-on-id)
  "Gets paths to nodes in a tree-on-id data-structure.

  See stash/tree-on-id function."
  (let ((paths (make-hash-table)))
    (labels ((inner (tree pid)
               (loop for k being the hash-keys in tree using (hash-value v)
                     do
                        (setf (gethash k paths) (append (gethash pid paths nil)
                                                        (list k)))
                        (when (gethash :children v)
                          (inner (gethash :children v) k)))))
      (inner tree-on-id 0))
    paths))

(defun stash/node-versions (node-id)
  "Gets all version of a node.

  stash currently only keeps upto 10 versions."
  (stash/invoke "node-versions" node-id))

(defun stash/get (&rest keys)
  "Gets value corresponding to a path of keys."
  (apply #'stash/invoke "get" keys))

(defun stash/keys (&rest parent-ids)
  "Gets keys under provided parent-ids.

  The root parent-id is 0."
  (apply #'stash/invoke "keys" parent-ids))

(defun stash/set (keys value)
  "Sets value of a path of keys."
  (apply #'stash/invoke "set" (append keys (list value))))

(defun stash/add (parent-id key value)
  "Adds a new node under a parent."
  (stash/invoke "add" parent-id key value))

(defun stash/rename (node-id new-name)
  "Renames a node."
  (stash/invoke "rename" node-id new-name))

(defun stash/update (node-id value)
  "Updates a node's value."
  (stash/invoke "update" node-id value))

(defun stash/delete (&rest node-ids)
  "Deletes nodes by ids."
  (apply #'stash/invoke "delete" node-ids))

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
          do
             (t:add-row table (list fn (short-function-documentation fn)))
          )
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
