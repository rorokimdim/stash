;; If `stash` command is not in your path, use the full path instead.
;; For example: (babashka.pods/load-pod ["/a/b/c/bin/stash"])
(babashka.pods/load-pod ["stash"])

(ns user
  (:require
   [clojure.java.io :as io]
   [table.core :as t]
   [pod.rorokimdim.stash :as stash])
  (:import [java.lang ProcessBuilder$Redirect]))

(defn stash-init
  "Initializes stash.

  The encryption key is read from STASH_ENCRYPTION_KEY environment variable.

  Stash directory is `.stash` in current directory. If it does not exis, it will be created."
  []
  (let [ekey (System/getenv "STASH_ENCRYPTION_KEY")]
    (stash/init {"encryption-key" ekey
                 "stash-directory" ".stash"
                 "create-stash-if-missing" true})))

(defn stash-nodes
  "Gets all nodes stored in stash.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  ([] (stash-nodes 0))
  ([parent-id] (stash/nodes parent-id)))

(defn stash-trees
  "Gets all nodes stored in stash as a list of trees.

  Simlar to stash-nodes but as nested structures (key -> [node, children]) rather than a list of nodes.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  ([] (stash-trees 0))
  ([parent-id] (stash/trees parent-id)))

(defn stash-node-versions
  "Gets all version of a node.

  stash currently only keeps upto 10 versions."
  [node-id]
  (stash/node-versions node-id))

(defn stash-get
  "Gets value corresponding to a path of keys."
  [& ks]
  (apply stash/get ks))

(defn stash-keys
  "Gets keys under provided parent-ids.

  The root parent-id is 0."
  [& pids]
  (apply stash/keys pids))

(defn stash-set
  "Sets value of a path of keys."
  [ks value]
  (apply stash/set (concat ks [value])))

(defn stash-add
  "Adds a new node under a parent."
  [parent-id k v]
  (stash/add parent-id k v))

(defn stash-update
  "Updates a node's value."
  [nid v]
  (stash/update nid v))

(defn stash-delete
  "Deletes nodes by ids."
  [& nids]
  (apply stash/delete nids))

(def editor-command (or (System/getenv "EDITOR") "vim"))
(defn edit []
  (let [f (io/file "user.clj")]
    (-> (ProcessBuilder. [editor-command (.getPath f)])
        (.inheritIO)
        (.start)
        (.waitFor))
    (load-string (slurp f))))

(defn startup []
  (if (not (stash-init))
    (println "☠️  Invalid encryption key. Failed to initialize stash.")
    (println "✔︎ stash initialized."))

  (let [f (io/file "user.clj")]
    (when (.exists f)
      (println "✔︎ Found user.clj. Loading...")
      (load-string (slurp f)))))

(defn shutdown []
  (println "Goodbye!"))

(-> (Runtime/getRuntime)
    (.addShutdownHook (Thread. shutdown)))

(startup)
