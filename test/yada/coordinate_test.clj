(ns yada.coordinate-test
  (:require [clojure.test :refer :all]
            [yada.coordinate :refer :all]))

(defn- almost= [a b]
  (< (Math/abs (- a b)) 1e-10))

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
  (are [c1 c2 result] (almost= result (distance c1 c2))
    {:x 0  :y 0} {:x 0 :y 1} 1.0
    {:x 0  :y 0} {:x 0 :y 2} 2.0
    {:x -2 :y 0} {:x 3 :y 0} 5.0
    {:x 0  :y 0} {:x 1 :y 1} (Math/sqrt 2)
    {:x -3 :y 8} {:x 5 :y 2} 10.0))

(deftest rad->deg-test
  (are [rad deg] (almost= deg (@#'yada.coordinate/rad->deg rad))
    Math/PI       180.0
    (* 2 Math/PI) 360.0
    (/ Math/PI 2) 90.0
    1             (/ 180.0 Math/PI)))

(deftest angle-test
  ; Tests from C version
  (let [a {:x 0 :y 0}
        b {:x 0 :y 1}
        c {:x 1 :y 0}
        d {:x (Math/sqrt 3) :y 0}]
    (is (almost= 90.0 (angle a b c)))
    (is (almost= 45.0 (angle b c a)))
    (is (almost= 45.0 (angle c a b)))
    (is (almost= 90.0 (angle a b d)))
    (is (almost= 60.0 (angle b d a)))
    (is (almost= 30.0 (angle d a b)))))
