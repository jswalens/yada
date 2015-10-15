(ns priority-queue
  (:refer-clojure :exclude [into pop])
  (:import [clojure.string]
           [java.util PriorityQueue]))

(defn create [cmp]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  (PriorityQueue. 11 cmp))

(defn push [queue val]
  "Push `val` in `queue`."
  (locking queue
    (.add queue val)))

(defn into [queue vals]
  "Push list of values `vals` into `queue`."
  (locking queue
    (doseq [v vals]
      (push queue v))))

(defn pop [queue]
  "Pops element from `queue` and returns it."
  (locking queue
    (.poll queue)))
