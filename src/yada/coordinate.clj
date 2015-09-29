(ns yada.coordinate)

(defn compare [a b]
  "Compare two vectors `a` and `b` (lexicographically)."
  (cond
    (< (:x a) (:x b)) -1
    (> (:x a) (:x b)) +1
    (< (:y a) (:y b)) -1
    (> (:y a) (:y b)) +1
    :else              0))

(defn distance [a b]
  "Returns distance between `a` and `b`, i.e.
  sqrt((a_x - b_x)^2 + (a_y - b_y)^2)"
  (let [dx (double (- (:x a) (:x b)))
        dy (double (- (:y a) (:y b)))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn- rad->deg [rad]
  "Convert radians to degrees."
  (/ (* 180.0 (double rad)) Math/PI))

(defn angle [a b c]
  "Returns `alpha` (in degrees) in
  cos(alpha) = ((b - a) .* (c - a)) / (||b - a|| * ||c - a||)"
  (let [{ax :x ay :y} a
        {bx :x by :y} b
        {cx :x cy :y} c
        num   (+ (* (- bx ax) (- cx ax)) (* (- by ay) (- cy ay)))
        denom (* (distance a b) (distance a c))
        cos   (/ (double num) (double denom))
        alpha (Math/acos cos)]
    (rad->deg alpha)))
