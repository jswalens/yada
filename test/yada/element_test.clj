(ns yada.element-test
  (:require [clojure.test :refer :all]
            [yada.element :refer :all]
            [yada.coordinate :as coordinate]))

(deftest min-index-test
  (is
    (= 2
      (@#'yada.element/min-index coordinate/compare
        [{:x 1 :y 1} {:x 0 :y 1} {:x 0 :y 0}]))))

(deftest get-common-edge-test
  ; Fake data that tests whether the functionality is ok
  (= :test1 (get-common-edge {:edges [:test1 :test2 :test3]}
                             {:edges [:test4 :test1 :test5]}))
  (= nil    (get-common-edge {:edges [:test1 :test2 :test3]}
                             {:edges [:test4]})))
