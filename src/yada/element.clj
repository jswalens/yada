(ns yada.element
  (:require [yada.options :as options]
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

(defn- check-angles [coordinates]
  (if (= (count coordinates) 3)
    (let [angles (map (fn [[a b c]] (coordinate/angle a b c)) permutations)
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
    (println "ERROR: when allocating an element, it should have two or three coordinates.")))

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
                 (println "ERROR: expected two or three coordinates"))]
    (collect-maps
      (map #(get-edge-midpoint-radius coordinates %) (range n-edge)))))

(defn alloc [coordinates]
  (let [coordinates
          (minimize-coordinates coordinates)
        {:keys [skinny? encroached-edge min-angle]}
          (check-angles coordinates)
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
       :neighbors       [] ; list of refs to neighboring elements; TODO: use
                        ; element/list-compare to order
       :garbage?        false
       :referenced?     false})))

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
    (or (is-encroached? @element) (is-skinny? @element))))

(defn set-is-referenced? [element status]
  (= (:referenced? @element) status))

(defn add-neighbor [element neighbor]
  "Note: when calling (add-neighbor a b), don't forget to call
  (add-neighbor b a) as well."
  (dosync
    (alter element update-in [:neighbors] conj neighbor)))
