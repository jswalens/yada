(ns yada.coordinate-test
  (:require [clojure.test :refer :all]
            [yada.coordinate :refer :all]))

(deftest compare-test
  (are [c1 c2 result] (= result (compare c1 c2))
    {:x 0 :y 0} {:x 0 :y 0} 0
    {:x 0 :y 0} {:x 0 :y 1} -1
    {:x 0 :y 0} {:x 1 :y 0} -1
    {:x 0 :y 0} {:x 1 :y 1} -1
    {:x 0 :y 1} {:x 0 :y 0} +1
    {:x 0 :y 1} {:x 0 :y 1} 0
    {:x 0 :y 1} {:x 1 :y 0} -1
    {:x 0 :y 1} {:x 1 :y 1} -1
    {:x 1 :y 0} {:x 0 :y 0} +1
    {:x 1 :y 0} {:x 0 :y 1} +1
    {:x 1 :y 0} {:x 1 :y 0} 0
    {:x 1 :y 0} {:x 1 :y 1} -1
    {:x 1 :y 1} {:x 0 :y 0} +1
    {:x 1 :y 1} {:x 0 :y 1} +1
    {:x 1 :y 1} {:x 1 :y 0} +1
    {:x 1 :y 1} {:x 1 :y 1} 0))

(deftest distance-test
  (are [c1 c2 result] (= result (distance c1 c2))
    {:x 0  :y 0} {:x 0 :y 1} 1.0
    {:x 0  :y 0} {:x 0 :y 2} 2.0
    {:x -2 :y 0} {:x 3 :y 0} 5.0
    {:x 0  :y 0} {:x 1 :y 1} (Math/sqrt 2)
    {:x -3 :y 8} {:x 5 :y 2} 10.0))
