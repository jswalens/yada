(ns priority-queue
  (:refer-clojure :exclude [into pop])
  (:import [clojure.string] [java.util LinkedList]))

(defn create [cmp]
  "Create an empty priority queue. Its elements will be ordered by comparing the
  result of `(key element)`."
  {:cmp cmp :elements (LinkedList.)})

(defn push [queue val]
  "Push `val` in `queue`."
  (locking queue
    (let [elements (:elements queue)
          n        (.size elements)]
      (loop [i 0]
        (if (< i n)
          (case ((:cmp queue) (.get elements i) val)
            -1 ; old < val: keep going
              (recur (inc i))
             0 ; old == val: still keep going, val will be inserted AFTER
               ; the ones it is equal to
              (recur (inc i))
            +1 ; old > val: insert val here
              (.add elements i val))
          ; all <= val: insert val at end
          (.add elements i val))))))

(defn into [queue vals]
  "Push list of values `vals` into `queue`."
  (locking queue
    (doseq [v vals]
      (push queue v))))

(defn pop [queue]
  "Pops element from `queue` and returns it."
  (locking queue
    (.poll (:elements queue))))
