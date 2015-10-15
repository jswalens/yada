(ns priority-queue
  (:refer-clojure :exclude [into pop])
  (:import [clojure.string]
           [java.util.concurrent PriorityBlockingQueue]))

(defn create [cmp]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  (PriorityBlockingQueue. 11 cmp))

(defn push [queue val]
  "Push `val` in `queue`."
  (.add queue val))

(defn into [queue vals]
  "Push list of values `vals` into `queue`."
  (doseq [v vals]
    (push queue v)))

(defn pop [queue]
  "Pops element from `queue` and returns it."
  (.poll queue))
