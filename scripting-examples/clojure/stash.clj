;; If `stash` command is not in your path, use the full path instead.
(def ^{:doc "path to stash command"} stash-command "stash")

(require '[babashka.pods :as pods])
(pods/load-pod [stash-command])

(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [babashka.fs :as fs]
   [pp-grid.api :as g]
   [pod.rorokimdim.stash :as stash])
  (:import [java.lang ProcessBuilder$Redirect]))

(def editor-command (or (System/getenv "EDITOR") "vim"))

(defn edit-file
  "Opens given file in editor and returns the file contents after the editor exits.

  Does not work on windows."
  [path]
  (-> (ProcessBuilder. [editor-command path])
      (.inheritIO)
      (.start)
      (.waitFor))
  (slurp path))

(defn edit
  "Opens user.clj in editor and loads the content after editor exits.

  Does not work on windows."
  []
  (load-string (edit-file "user.clj")))

(defn docstrings
  "Gets sequence of maps with symbol-name and symbol-docstring."
  ([namespace] (docstrings namespace (constantly true) ""))
  ([namespace prefix] (docstrings namespace (constantly true) prefix))
  ([namespace pred prefix]
   (let [m  (into (sorted-map) (ns-publics namespace))]
     (for [[k v] m
           :when (pred k v)]
       {:name (str prefix k) :description (:doc (meta v))}))))

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
       (#(g/table [:name :description] % :align :left))
       str
       println))

(defn help
  "Pretty prints all docstrings of functions in stash."
  []
  (pretty-print-docstrings (docstrings 'pod.rorokimdim.stash)))

(defn load-stash
  "Loads stash file from given path. Returns true on success, false on error."
  ([path encryption-key] (load-stash path encryption-key true))
  ([path encryption-key create-stash-if-missing]
   (try
     (stash/init {:encryption-key encryption-key
               :stash-path path
               :create-stash-if-missing create-stash-if-missing})
     (catch Exception e
       (let [type (keyword (:type (ex-data e)))
             message (.getMessage e)]
         (println (str "☠️  " type "\n" message))
         false)))))

(defn startup
  "Sets up the repl.

  1. loads user.clj if it exists
  2. prints some help"
  []
  (println (str "Stash v" (stash/version)))
  (let [f (io/file "user.clj")]
    (when (.exists f)
      (println " ✔︎ Found user.clj. Loading...")
      (load-string (slurp f))))

  (println)
  (println "▸ Available functions from stash")
  (println "  Use doc function for more information.")
  (-> (docstrings 'pod.rorokimdim.stash "stash/")
      tabulate-docstrings)

  (println "▸ Other functions in this repl")
  (println "  Use doc function for more information.")
  (-> (docstrings 'user
                  (fn [k _v]
                    (#{"edit"
                       "help"
                       "load-stash"} (str k)))
                  "")
      tabulate-docstrings))

(defn shutdown
  "Shutdown hook."
  []
  (println "Goodbye!"))

(-> (Runtime/getRuntime)
    (.addShutdownHook (Thread. shutdown)))

(startup)
