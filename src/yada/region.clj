(ns yada.region
  (:require [priority-queue]
            [yada.options :as options :refer [log error reduce-all]]
            [yada.element :as element]
            [yada.mesh :as mesh]
            [taoensso.timbre.profiling :refer [p defnp]]))

(defnp retriangulate [element mesh visited borders]
  "Returns [n-inserted new-bad-elements]."
  (let [center-coordinate
          (element/get-new-point element)
        ; Remove the old triangles
        _
          (p :retriangulate-remove
            (doseq [v visited]
              (mesh/remove-element mesh v)))
        ; If segment is encroached, split it in half
        segment-encroached?
          (= (element/get-num-edge element) 1)
        _
          (p :retriangulate-split
            (when (= (element/get-num-edge element) 1)
              (let [edge (element/get-edge element 0)
                    a    (element/alloc [center-coordinate (:first edge)])
                    b    (element/alloc [center-coordinate (:second edge)])]
                (mesh/insert-element mesh a)
                (mesh/insert-element mesh b)
                (mesh/remove-boundary mesh (element/get-edge element 0))
                (mesh/insert-boundary mesh (element/get-edge a 0))
                (mesh/insert-boundary mesh (element/get-edge b 0)))))
        ; Insert the new triangles. These are constructed using the new point
        ; and the two points from the border segment.
        after-elements
          (map
            #(element/alloc [center-coordinate (:first %) (:second %)])
            borders)
        _
          (p :retriangulate-insert
            (doseq [after after-elements]
              (mesh/insert-element mesh after)))
        new-bad-elements
          (filter element/bad? after-elements)]
    (log "Removed " (count visited) " visited, added "
      (if segment-encroached? 2 0) " because encroached?, added "
      (count borders) " borders.")
    [(+
       (- (count visited))
       (if segment-encroached? 2 0)
       (count borders))
     new-bad-elements]))

(defnp visit-neighbors [mesh current center-element visited]
  (let [neighbors         (:neighbors @current)
        boundary?         (= (element/get-num-edge center-element) 1)
        center-coordinate (element/get-new-point center-element)
        results
          (for [neighbor (remove #(.contains visited %) neighbors)]
            (if (element/in-circum-circle? neighbor center-coordinate)
              ; This element is part of the region:
              (if (and (not boundary?) (= (element/get-num-edge neighbor) 1))
                ; It encroaches on the mesh boundary, so we'll have to split it and
                ; restart:
                {:encroached neighbor}
                ; Continue breadth-first search:
                {:to-expand neighbor})
              ; This element is not part of the region, so it borders the region.
              ; Save its info for retriangulation.
              (let [border-edge (element/get-common-edge @neighbor @current)]
                ; C version says here: if no border-edge found: restart the tx.
                ; As far as I can see, this error is not possible in Clojure's STM.
                {:border border-edge})))]
    {:encroached  (->> results (map :encroached) (filter some?) (first))
     :to-expand   (->> results (map :to-expand)  (filter some?))
     :new-borders (->> results (map :border)     (filter some?))}))

(defnp grow-region [mesh center-element]
  "Returns either `{:encroached encroached-neighbor}` or
  `{:encroached nil :visited visited :borders borders}`."
  (loop [expand-queue [center-element]
         visited      #{}
         borders      #{}]
    (if-let [current (first expand-queue)]
      (if (contains? visited current)
        (recur (rest expand-queue) visited borders)
        (let [new-visited (conj visited current)
              {:keys [encroached to-expand new-borders]}
                (visit-neighbors mesh current center-element new-visited)]
          (if encroached
            {:encroached encroached}
            (recur
              (into (rest expand-queue) to-expand) ; will never have duplicates
              new-visited
              (into borders new-borders)))))
      {:encroached nil :visited visited :borders borders})))

(defnp refine-helper [element mesh]
  "Returns `{:n number of inserted elements :bad new bad elements
    :visited old visited elements :borders new border elements}`."
  (dosync
    (let [{:keys [n-refine bad visited borders]}
            (loop [n-refine 0
                   bad      []
                   visited  []
                   borders  []]
              (if (element/garbage? element)
                {:n-refine n-refine :bad bad :visited visited :borders borders}
                (let [res (grow-region mesh element)
                      encroached (:encroached res)]
                  (if encroached
                    (let [{:keys [n bad visited borders]}
                            (refine-helper encroached mesh)]
                      (recur (+ n-refine n) bad visited borders))
                    {:n-refine n-refine
                     :bad      bad
                     :visited  (:visited res)
                     :borders  (:borders res)}))))
          [n-retriangulate new-bad-elements]
            (if (element/garbage? element)
              [0 nil]
              (retriangulate element mesh visited borders))]
      {:n       (+ n-refine n-retriangulate)
       :bad     (into bad (remove element/garbage? new-bad-elements))
       :visited visited
       :borders borders})))

(defnp refine [element mesh]
  "Refine the region around element, i.e. remove element and replace it with
  something better.

  Returns `{:n number of inserted elements :bad new (non-garbage) bad elements}`."
  (dosync
    (let [{n :n bad :bad} (refine-helper element mesh)]
      {:n n :bad bad})))
