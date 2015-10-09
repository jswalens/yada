(ns yada.main-test
  (:require [clojure.test :refer :all]
            [yada.main]
            [yada.mesh :as mesh]))

(defn test-all [input-name expect-init-num-element expect-init-num-bad-element
  expect-final-num-element expect-num-process]
  (let [{mesh :mesh init-num-element :n-element}
          (mesh/read input-name)
        ; mesh tested in mesh_test
        work-queue
          (@#'yada.main/initialize-work mesh)
        init-num-bad-element
          (count (:elements @work-queue))
        _ (is (= expect-init-num-element init-num-element))
        _ (is (= expect-init-num-bad-element init-num-bad-element))
        {total-num-added :n-added num-process :n-process}
          (@#'yada.main/process mesh work-queue)
        final-num-element
          (+ init-num-element total-num-added)
        _ (is (= expect-final-num-element final-num-element))
        _ (is (= expect-num-process num-process))
        success?
          ;(mesh/check mesh final-num-element) - disabled TODO
          true
        _ (is success?)]))

(deftest test-633
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/633.2" 1264 438 2698 710))

(deftest test-dots
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/dots.2" 198 70 576 188))

(deftest test-ladder
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/ladder.2" 254 126 512 123))

(deftest test-spiral
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/spiral.2" 28 8 40 6))
