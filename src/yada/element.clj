(ns yada.element
  (:require [clojure.string]
            [yada.options :as options :refer [log error]]
            [yada.coordinate :as coordinate]))

(defn- min-index [cmp lst]
  "Returns the index of the smallest element in `lst`, using comparison function
  `cmp`."
  (->> lst
    (map-indexed (fn [i v] [i v]))
    (sort (fn [[i1 v1] [i2 v2]] (cmp v1 v2)))
    (first)   ; get first [i v] pair
    (first))) ; get i of that pair

(defn- minimize-coordinates [coordinates]
  "Re-order `coordinates` so that the first element is the smallest coordinate."
  (let [min-position (min-index coordinate/compare coordinates)]
    ; Rotate the list of coordinates so that min-position is first
    (concat (drop min-position coordinates) (take min-position coordinates))))

(def permutations
  [[0 1 2]
   [1 2 0]
   [2 0 1]])

(defn- check-coordinates [coordinates]
  (if (= (count coordinates) 3)
    (let [angles
            (map
              (fn [[a b c]]
                (coordinate/angle (nth coordinates a) (nth coordinates b)
                  (nth coordinates c)))
              permutations)
          obtuse-angle-index
            (first (keep-indexed #(if (> %2 90.0) %1) angles))
            ; there will be at most one angle > 90 (geometric property, sum of
            ; angles in a triangle is 180°)
          encroached-edge-index
            (if (some? obtuse-angle-index)
              (mod (+ obtuse-angle-index 1) 3))]
      {:skinny?         (some #(< % options/angle-constraint) angles)
       :encroached-edge encroached-edge-index
       :min-angle       (first (sort angles))})
    {:skinny? false :encroached-edge nil :min-angle 180.0}))

(defn- sq [x]
  "Square `x`."
  (* x x))

(defn- calculate-circumcenter [coordinates]
  (case (count coordinates)
    2
      (let [[{ax :x ay :y} {bx :x by :y}] coordinates]
        {:x (/ (+ ax bx) 2.0)
         :y (/ (+ ay by) 2.0)})
    3
      (let [[{ax :x ay :y} {bx :x by :y} {cx :x cy :y}] coordinates
            deltab_x (- bx ax)
            deltab_y (- by ay)
            deltac_x (- cx ax)
            deltac_y (- cy ay)
            distb_2  (+ (sq deltab_x) (sq deltab_y))
            distc_2  (+ (sq deltac_x) (sq deltac_y))
            num_x    (- (* deltab_y distc_2) (* deltac_y distb_2))
            num_y    (- (* deltab_x distc_2) (* deltac_x distb_2))
            denom    (* 2 (- (* deltab_x deltac_y) (* deltac_x deltab_y)))
            rx       (- ax (/ num_x denom))
            ry       (+ ay (/ num_y denom))]
        {:x rx
         :y ry})
    (error "when allocating an element, it should have two or three coordinates.")))

(defn- calculate-circumcircle [coordinates]
  (let [circum-center (calculate-circumcenter coordinates)]
    {:circum-center circum-center
     :circum-radius (coordinate/distance circum-center (first coordinates))}))

(defn- create-edge [fst snd]
  "Create edge from coordinate `fst` to coordinate `snd`. Makes sure that
  the 'smallest' coordinate is put first, to make comparisons possible."
  (if (< (coordinate/compare fst snd) 0)
    {:first fst :second snd}   ; fst < snd
    {:first snd :second fst})) ; snd < fst
    ; note: fst == snd should be impossible, that would be an edge to itself.

(defn- get-edge-midpoint-radius [coordinates i]
  (let [fst      (nth coordinates i)
        snd      (nth coordinates (mod (inc i) (count coordinates)))
        edge     (create-edge fst snd)
        midpoint {:x (/ (+ (:x fst) (:x snd)) 2.0)
                  :y (/ (+ (:y fst) (:y snd)) 2.0)}
        radius   (coordinate/distance fst midpoint)]
    {:edges     edge
     :midpoints midpoint
     :radii     radius}))

(defn- collect-maps [maps]
  "Converts a list of `maps` into a map of lists.
  E.g. (collect-maps [{:a 1 :b 2} {:a 3 :b 4}]) = {:a [1 3] :b [2 4]}"
  (reduce
    (fn [res map]
      (reduce (fn [res [k v]] (update-in res [k] #(conj (vec %) v))) res map))
    {}
    maps))

(defn- get-edges-midpoints-radii [coordinates]
  (let [n-edge (case (count coordinates)
                 2 1
                 3 3
                 (error "expected two or three coordinates"))]
    (collect-maps
      (map #(get-edge-midpoint-radius coordinates %) (range n-edge)))))

(defn alloc [coordinates]
  (let [coordinates
          (minimize-coordinates coordinates)
        {:keys [skinny? encroached-edge min-angle]}
          (check-coordinates coordinates)
        {:keys [circum-center circum-radius]}
          (calculate-circumcircle coordinates)
        {:keys [edges midpoints radii]}
          (get-edges-midpoints-radii coordinates)]
    (ref
      {:coordinates     coordinates
       :circum-center   circum-center
       :circum-radius   circum-radius
       :min-angle       min-angle
       :edges           edges     ; 1 edge if 2 coordinates; 3 edges if 3 coords
       :midpoints       midpoints ; midpoint of each edge
       :radii           radii     ; half of edge length
       :encroached-edge encroached-edge ; index of the encroached edge in :edges,
                        ; nil if there is no encroached edge. The encroached edge
                        ; is the one opposite the obtuse angle of the triangle.
       :skinny?         skinny?
       :neighbors       #{}       ; refs to neighboring elements
       :garbage?        false
       :referenced?     false}))) ; TODO: we don't need this in Clojure as it's GC'd, I think

(defn get-num-edge [element]
  "Returns the number of edges of `element`."
  (count (:edges @element)))

(defn get-edge [element i]
  "Returns i'th edge of `element`."
  (nth (:edges @element) i))

(defn priority-queue-compare [a b]
  "Compare elements `a` and `b`, for sorting in a priority queue."
  (if (:encroached-edge a)
    (if (:encroached-edge b)
      0
      1)
    (if (:encroached-edge b)
      -1
      0)))

(defn is-in-circum-circle? [element coordinate]
  (dosync
    (<= (coordinate/distance coordinate (:circum-center @element))
        (:circum-radius @element))))

(defn- is-encroached? [element]
  (some? (:encroached-edge @element)))

(defn clear-encroached [element]
  (dosync
    (alter element assoc :encroached-edge nil)))

(defn is-skinny? [element]
  (:skinny? @element))

(defn is-bad? [element]
  "Does `element` need to be refined?"
  (dosync
    (or (is-encroached? element) (is-skinny? element))))

(defn is-referenced? [element status]
  (= (:referenced? @element) status))

(defn set-is-referenced? [element status]
  (dosync
    (alter element assoc :referenced? status)))

(defn is-garbage? [element]
  (:garbage? @element))

(defn set-is-garbage? [element status]
  (dosync
    (alter element assoc :garbage? status)))

(defn add-neighbor [element neighbor]
  "Add `neighbor` to `element`.

  Note: when calling `(add-neighbor a b)`, don't forget to call
  `(add-neighbor b a)` as well."
  (dosync
    (alter element update-in [:neighbors] conj neighbor)))

(defn remove-neighbor [element neighbor]
  "Remove `neighbor` for this `element`.

  Note: when calling `(remove-neighbor a b)`, don't forget to call
  `(remove-neighbor b a)` as well."
  (dosync
    (when (not (.contains (:neighbors @element) neighbor))
      (error "trying to remove a neighbor that doesn't exist"))
    (alter element update-in [:neighbors] disj neighbor)))

(defn get-common-edge [element-a element-b]
  (first
    (for [edge-a (:edges element-a)
          edge-b (:edges element-b)
          :when (= edge-a edge-b)]
      edge-a)))

(defn get-new-point [element]
  "`element` is encroached or skinny, so we create a new point to add.
  If the element is skinny, this is its circumcenter; if it's encroached, this
  is the midpoint of the encroached edge."
  (dosync
    (if (nil? (:encroached-edge @element))
      ; If it's skinny, new point = the circumcenter
      (:circum-center @element)
      ; If it's encroached, new point = the midpoint of the encroached edge
      (nth (:midpoints @element) (:encroached-edge @element)))))

(defn check-angles [element]
  (if (= (count (:coordinates @element)) 3)
    (not (:skinny? (check-coordinates (:coordinates @element))))
    true))

(defn element->str [element]
  (->> (:coordinates @element)
    (map coordinate/coordinate->str)
    (clojure.string/join " ")))

(defn elements->str [elements]
  (clojure.string/join "; " (map element->str elements)))
