;; If `stash` command is not in your path, use the full path instead.
(def ^{:doc "path to stash command"} stash-command "stash")

(def ^{:doc "path to stash file"} stash-file-path "demo.stash")

(babashka.pods/load-pod [stash-command])

(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [table.core :as t]
   [pod.rorokimdim.stash :as stash])
  (:import [java.lang ProcessBuilder$Redirect]))


(defn stash-init
  "Initializes stash.

  The encryption key is read from STASH_ENCRYPTION_KEY environment variable.

  If `stash-file-path` does not exist, it will be created."
  []
  (let [ekey (System/getenv "STASH_ENCRYPTION_KEY")]
    (stash/init {"encryption-key" ekey
                 "stash-path" stash-file-path
                 "create-stash-if-missing" true})))

(defn stash-version
  "Gets version of stash command."
  []
  (stash/version))

(defn stash-nodes
  "Gets all nodes stored in stash.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  ([] (stash-nodes 0))
  ([parent-id] (stash/nodes parent-id)))

(defn stash-tree
  "Gets all nodes stored in stash as a tree.

  Returns a hash-map of the form {key [node-id value child-tree]}.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  ([] (stash-tree 0))
  ([parent-id] (stash/tree parent-id)))

(defn stash-tree->stash-tree-on-id
  "Converts a stash-tree to an equivalent tree indexed on node-ids."
  [stree]
  (apply merge
         (for [[k [node-id node-value child-stree]] (into [] stree)]
           {node-id
            {:key k
             :value node-value
             :children (if (empty? child-stree)
                         {}
                         (stash-tree->stash-tree-on-id child-stree))}})))

(defn stash-tree-on-id
  "Gets all nodes stored in stash as a tree indexed by node-ids.

  Returns a hash-map of the form {id {:key key :value value :children child-tree}}.

  If a parent-node-id is provided, only nodes with that parent-id are returned."
  ([] (stash-tree-on-id 0))
  ([parent-id]
   (stash-tree->stash-tree-on-id (stash-tree parent-id))))

(defn stash-tree-on-id->paths [tree-on-id]
  "Gets paths to nodes in a tree-on-id data-structure.

  See stash-tree-on-id function."
  (let [paths (atom {})
        inner (fn f [t pid]
                (doseq [[nid {children :children}] (into [] t)]
                  (swap! paths assoc nid (conj (get @paths pid []) nid))
                  (if (seq children)
                    (f children nid))))]
    (inner tree-on-id 0)
    @paths))

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

(defn stash-rename
  "Renames a node."
  [nid new-name]
  (stash/rename nid new-name))

(defn stash-update
  "Updates a node's value."
  [nid v]
  (stash/update nid v))

(defn stash-delete
  "Deletes nodes by ids."
  [& nids]
  (apply stash/delete nids))

(defn stash-browse
  "Launches stash terminal-ui.

  Does not currently work on windows."
  []
  (-> (ProcessBuilder. [stash-command "browse" stash-file-path])
      (.inheritIO)
      (.start)
      (.waitFor))
  (stash-tree))

(def editor-command (or (System/getenv "EDITOR") "vim"))
(defn edit
  "Opens user.clj in editor and loads the content after editor exits.

  Does not currently work on windows."
  []
  (let [f (io/file "user.clj")]
    (-> (ProcessBuilder. [editor-command (.getPath f)])
        (.inheritIO)
        (.start)
        (.waitFor))
    (load-string (slurp f))))

(defn docstrings
  "Gets sequence of maps with symbol-name and symbol-docstring."
  ([namespace] (docstrings namespace (constantly true)))
  ([namespace pred]
   (let [m  (into (sorted-map) (ns-publics namespace))]
     (for [[k v] m
           :when (pred k v)]
       {:name (str k) :description (:doc (meta v))}))))

(defn pretty-print-docstrings
  "Pretty-prints maps from `docstrings` function."
  [ms]
  (doseq [{name :name description :description} ms]
    (println (str "▸ " name))
    (when (not (nil? description))
      (println (->> description
                    s/split-lines
                    (map s/trim)
                    (s/join "\n")))
      (println))))

(defn tabulate-docstrings
  "Prints maps from `docstrings` function as a table.

  Only the first line of each description will be shown."
  [ms]
  (->> ms
       (map (fn [m] (update-in m [:description] #(and %1 (first (s/split-lines %1))))))
       (#(t/table % :fields [:name :description]))))

(defn stash-help
  "Prints docstrings for stash symbols."
  []
  (-> (docstrings 'user (fn [k _] (s/starts-with? k "stash-")))
      pretty-print-docstrings))

(defn startup
  "Sets up the repl.

  1. calls stash-init
  2. prints some help
  3. loads user.clj if it exists"
  []
  (if (not (stash-init))
    (do
      (println (str "☠️  Invalid encryption key for \"" stash-file-path "\". Failed to initialize stash."))
      (println (str "\nPlease delete \"" stash-file-path "\" to create one with a different key.\n"))
      (System/exit 1))
    (do
      (println "✔︎ stash initialized.")
      (println "Try these functions:")
      (-> (docstrings 'user (fn [k v] (and (s/starts-with? k "stash-")
                                           (fn? @v))))
          tabulate-docstrings)))

  (let [f (io/file "user.clj")]
    (when (.exists f)
      (println "✔︎ Found user.clj. Loading...")
      (load-string (slurp f)))))

(defn shutdown
  "Shutdown hook."
  []
  (println "Goodbye!"))

(-> (Runtime/getRuntime)
    (.addShutdownHook (Thread. shutdown)))

(startup)
