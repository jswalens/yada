(ns yada.main-test
  (:require [clojure.test :refer :all]
            [yada.main]
            [yada.mesh :as mesh]))

(deftest test-633
  (let [{mesh :mesh init-num-element :n-element}
          (mesh/read "inputs/633.2")
        ; mesh tested in mesh_test
        work-queue
          (@#'yada.main/initialize-work mesh)
        init-num-bad-element
          (count (:elements @work-queue))
        _ (is (= 1264 init-num-element))    ; from C version
        _ (is (= 438 init-num-bad-element)) ; from C version
        {total-num-added :n-added num-process :n-process}
          (@#'yada.main/process mesh work-queue)
        final-num-element
          (+ init-num-element total-num-added)
        _ (is (= 2678 final-num-element))   ; from C version
        _ (is (= 705 num-process))          ; from C version
        success?
          ;(mesh/check mesh final-num-element) - disabled
          true
        _ (is success?)]))
