(ns priority-queue)

(defn create [key]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  {:key key :elements (list)})

(defn add [queue val]
  "Add `val` to `queue`."
  (assoc queue :elements
    (sort-by (:key queue) (cons val (:elements queue)))))

;(defn remove [queue val]
;  "Remove `val` from `queue`."
;  (clojure.core/remove #(= % val) queue))
