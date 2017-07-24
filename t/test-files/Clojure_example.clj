(defn quick-sort [[pivot & coll]]
  (when pivot
    (let [greater? #(> % pivot)]
      (lazy-cat (quick-sort (remove greater? coll))
                [pivot]
                (quick-sort (filter greater? coll))))))