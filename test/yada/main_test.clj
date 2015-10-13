(ns yada.main-test
  (:require [clojure.test :refer :all]
            [yada.main]
            [yada.mesh :as mesh]))

(defn test-all [input-name expect-init-n-element expect-init-n-bad-element
  expect-final-n-element expect-n-processed]
  (let [{mesh :mesh init-n-element :n-element}
          (mesh/read input-name)
        ; mesh tested in mesh_test
        work-queue
          (@#'yada.main/initialize-work mesh)
        init-n-bad-element
          (count (:elements @work-queue))
        _ (is (= expect-init-n-element init-n-element))
        _ (is (= expect-init-n-bad-element init-n-bad-element))
        {final-n-element :n-element n-processed :n-processed}
          (@#'yada.main/process mesh init-n-element work-queue)
        _ (is (= expect-final-n-element final-n-element))
        _ (is (= expect-n-processed n-processed))
        success?
          (mesh/check mesh final-n-element false)
        _ (is success?)]))

(deftest test-spiral
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/spiral.2" 28 8 40 6))

(deftest test-dots
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/dots.2" 198 70 576 188))

(deftest test-ladder
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/ladder.2" 254 126 512 123))

(deftest test-633
  ; Expected results from C version, without shuffling the bad queue in
  ; yada.c/initializeWork
  (test-all "inputs/633.2" 1264 438 2698 710))
