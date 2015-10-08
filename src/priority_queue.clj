(ns priority-queue)

(defn create [cmp]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  (ref {:cmp cmp :elements (list)}))

(defn push [queue val]
  "Push `val` in `queue`."
  (dosync
    (alter queue assoc :elements
      (sort (:cmp @queue) (cons val (:elements @queue))))))

(defn pop [queue]
  "Pops element from `queue` and returns it.
  Assumes `queue` is a ref around the priority queue."
  (dosync
    (when-let [e (first (:elements @queue))]
      (alter queue update-in [:elements] rest)
      e)))

;(defn remove [queue val]
;  "Remove `val` from `queue`."
;  (clojure.core/remove #(= % val) queue))
