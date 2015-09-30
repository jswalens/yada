(ns yada.mesh-test
  (:require [clojure.test :refer :all]
            [yada.mesh :refer :all]))

(deftest parse-line-test
  (is
    (= (list 1 2.3)
      (@#'yada.mesh/parse-line "1 2.3" @#'yada.mesh/str->int @#'yada.mesh/str->double))))

(deftest read-test
  (is (= 1264 (:n-element (read "inputs/633.2")))))
