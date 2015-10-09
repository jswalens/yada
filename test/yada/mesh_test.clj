(ns yada.mesh-test
  (:require [clojure.test :refer :all]
            [yada.mesh :as mesh]
            [yada.element :as element]))

(defn- almost= [a b]
  (< (Math/abs (- a b)) 1e-5))

(deftest parse-line-test
  (is
    (= (list 1 2.3)
      (@#'yada.mesh/parse-line "1 2.3" @#'yada.mesh/str->int @#'yada.mesh/str->double))))

(deftest read-test
  ; Expect results from C version
  (let [{:keys [n-element mesh]}
          (mesh/read "inputs/633.2")
        root-element @(:root-element @mesh)]
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
    (is (= (list {:x 570110.0 :y 431911.0} {:x 585233.0 :y 413468.0} {:x 584373.0 :y 414654.0})
           (:coordinates @(first (:init-bad-queue @mesh)))))))

(deftest insert-remove-boundary-test
  (let [{:keys [n-element mesh]}
          (mesh/read "inputs/633.2")
        get-n-boundaries #(count (:boundary-set @mesh))
        root             (:root-element @mesh)
        init-boundaries  (get-n-boundaries)]
    (is (.contains (:boundary-set @mesh) (element/get-edge root 0)))
    ; duplicate insert
    (mesh/insert-boundary mesh (element/get-edge root 0))
    (is (= init-boundaries (get-n-boundaries)))
    (mesh/remove-boundary mesh (element/get-edge root 0))
    (is (= (- init-boundaries 1) (get-n-boundaries)))
    ; new insert
    (mesh/insert-boundary mesh (element/get-edge root 0))
    (is (= init-boundaries (get-n-boundaries)))
    ; duplicate insert
    (let [fst (first (:boundary-set @mesh))]
      (mesh/insert-boundary mesh fst)
      (is (= init-boundaries (get-n-boundaries)))
      (mesh/remove-boundary mesh fst)
      (is (= (- init-boundaries 1) (get-n-boundaries))))))

(deftest get-bad-test
  (let [{:keys [n-element mesh]}
          (mesh/read "inputs/633.2")
        n         (count (:init-bad-queue @mesh))
        first-bad (mesh/get-bad mesh)]
    (is (element/is-bad? first-bad))
    (is (= (- n 1) (count (:init-bad-queue @mesh))))))
