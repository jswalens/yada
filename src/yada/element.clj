(ns yada.element)

(defn compare [a b]
  "Compare elements `a` and `b`, for sorting in a priority queue."
  (if (:encroached-edge a)
    (if (:encroached-edge b)
      0
      1)
    (if (:encroached-edge b)
      -1
      0)))

(defn set-is-referenced? [element status]
  (= (:is-referenced element) status))
