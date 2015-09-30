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
