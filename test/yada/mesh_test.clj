(ns yada.mesh-test
  (:require [clojure.test :refer :all]
            [yada.mesh :refer :all]))

(defn- almost= [a b]
  (< (Math/abs (- a b)) 1e-5))

(deftest parse-line-test
  (is
    (= (list 1 2.3)
      (@#'yada.mesh/parse-line "1 2.3" @#'yada.mesh/str->int @#'yada.mesh/str->double))))

(deftest read-test
  (let [{:keys [n-element mesh]} (read "inputs/633.2")
        root-element   @(:root-element @mesh)
        first-in-queue (first (:init-bad-queue @mesh))]
    (is (= 1264 n-element))
    (is (= 438  (count (:init-bad-queue @mesh))))
    (is (= (list {:x 570110.0 :y 431911.0} {:x 585233.0 :y 413468.0})
           (:coordinates root-element)))
    (is (= {:x 577671.5 :y 422689.5} (:circum-center root-element)))
    (is (almost= 11925.281737        (:circum-radius root-element)))
    (is (= 180.0                     (:min-angle root-element)))
    (is (= 1                         (count (:edges root-element))))
    (is (= [{:first {:x 570110.0 :y 431911.0} :second {:x 585233.0 :y 413468.0}}]
           (:edges root-element)))
    (is (= false                     (:skinny? root-element)))
    (is (= false                     (:garbage? root-element)))
    (is (= false                     (:referenced? root-element)))
    ;(is (= (list {:x 570110.0 :y 431911.0} {:x 585233.0 :y 413468.0} {:x 584400 :y 414700})
    ;       (:coordinates first-in-queue))) -- FIXME: last coordinate inexact
    ))
