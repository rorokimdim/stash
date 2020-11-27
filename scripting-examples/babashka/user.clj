(defn show-stash-keys
  "Shows stash-keys as a table."
  ([] (show-stash-keys 0))
  ([& pids] (let [ks (apply stash-keys pids)]
              (t/table ks))))
