(ns yada.element-test
  (:require [clojure.test :refer :all]
            [yada.element :refer :all]
            [yada.coordinate :as coordinate]))

(deftest min-index-test
  (is
    (= 2
      (@#'yada.element/min-index coordinate/compare
        [{:x 1 :y 1} {:x 0 :y 1} {:x 0 :y 0}]))))
