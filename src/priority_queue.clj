(ns priority-queue)

(defn create [cmp]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  (ref {:cmp cmp :elements []}))

(defn push [queue val]
  "Push `val` in `queue`."
  (dosync
    (let [new-elements
            (loop [old-elements (:elements @queue)
                   new-elements []]
              (let [[fst & rst] old-elements]
                (if (nil? fst)
                  (conj new-elements val)
                  (case ((:cmp @queue) fst val)
                    -1 ; old < val: continue
                      (recur rst (conj new-elements fst))
                     0 ; old == val: still continue, val will be inserted AFTER
                       ; the ones it is equal to
                      (recur rst (conj new-elements fst))
                    +1 ; old > val: insert it and end the loop
                      (into (conj new-elements val) old-elements)))))]
      (alter queue assoc :elements new-elements))))

(defn pop [queue]
  "Pops element from `queue` and returns it.
  Assumes `queue` is a ref around the priority queue."
  (dosync
    (when-let [e (first (:elements @queue))]
      (alter queue update-in [:elements] rest)
      e)))
