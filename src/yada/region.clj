(ns yada.region
  (:require [priority-queue]
            [yada.element :as element]
            [yada.mesh :as mesh]))

;(def log println)
(defn log [& _] nil)

(defn- reduce-all [f a l]
  (doall (reduce f a l)))

(defn retriangulate [element mesh visited borders edge-map]
  "Returns [n-inserted new-bad-elements]."
  (let [center-coordinate
          (element/get-new-point element)
        ; Remove the old triangles
        edge-map
          (reduce-all
            (fn [edge-map v]
              (mesh/remove-element mesh v)
              (mesh/remove-element-from-edge-map edge-map v))
            edge-map
            visited)
        ; If segment is encroached, split it in half
        segment-encroached?
          (= (element/get-num-edge element) 1)
        edge-map
          (if (= (element/get-num-edge element) 1)
            (let [edge (element/get-edge element 0)
                  a    (element/alloc [center-coordinate (:first edge)])
                  b    (element/alloc [center-coordinate (:second edge)])
                  edge-map (mesh/insert-element mesh a edge-map)
                  edge-map (mesh/insert-element mesh b edge-map)
                  _    (mesh/remove-boundary mesh (element/get-edge element 0))
                  _    (mesh/insert-boundary mesh (element/get-edge a 0))
                  _    (mesh/insert-boundary mesh (element/get-edge b 0))]
              edge-map)
            edge-map)
        ; Insert the new triangles. These are constructed using the new point
        ; and the two points from the border segment.
        after-elements
          (map
            #(element/alloc [center-coordinate (:first %) (:second %)])
            borders)
        _
          (reduce-all
            (fn [edge-map after]
              (mesh/insert-element mesh after edge-map))
            edge-map
            after-elements)
        new-bad-elements
          (filter element/is-bad? after-elements)]
    (log "Removed" (count visited) "visited, added"
      (if segment-encroached? 2 0) "because encroached?, added" (count borders)
      "borders")
    [(+
       (- (count visited))
       (if segment-encroached? 2 0)
       (count borders))
     new-bad-elements]))

(defn- elements->str [elements]
  (->> elements
    (map element/element->str)
    (clojure.string/join "\n")))

(defn- visit-neighbors [current center-element visited borders edge-map]
  (let [neighbors         (:neighbors @current)
        boundary?         (= (element/get-num-edge center-element) 1)
        center-coordinate (element/get-new-point center-element)]
    (log "Visiting neighbors...")
    (reduce-all
      (fn [{:keys [encroached to-expand borders edge-map] :as m} neighbor]
        (log "Visiting neighbor" (element/element->str neighbor))
        ;TODO: (element/is-garbage? neighbor) ; so we can detect conflicts
        (if (element/is-in-circum-circle? neighbor center-coordinate)
          ; This element is part of the region:
          (if (and (not boundary?) (= (element/get-num-edge neighbor) 1))
            ; It encroaches on the mesh boundary, so we'll have to split it and
            ; restart:
            (assoc m :encroached neighbor)
            ; Continue breadth-first search:
            (update-in m [:to-expand] conj neighbor))
          ; This element is not part of the region, so it borders the region.
          ; Save its info for retriangulation.
          (let [border-edge (element/get-common-edge @neighbor @current)]
            ; TODO: if no border edge: tx restart - can this happen in Clojure's STM?
            (when (.contains borders border-edge)
              (println "ERROR: duplicate in borders" border-edge))
            (-> m
              (update-in [:borders] conj border-edge)
              (update-in [:edge-map] #(mesh/put-in-edge-map-if-empty % border-edge neighbor))))))
      {:encroached nil :to-expand [] :borders borders :edge-map edge-map}
      (remove #(.contains visited %) neighbors))))

(defn grow-region [center-element]
  "Returns either `{:encroached encroached-neighbor}` or
  `{:encroached nil :visited visited :borders borders :edge-map edge-map}`."
  (loop [expand-queue [center-element]
         visited      #{}
         borders      #{}
         edge-map     {}]
    (if-let [current (first expand-queue)]
      (if (contains? visited current)
        (recur (rest expand-queue) visited borders edge-map)
        (let [new-visited (conj visited current)
              {:keys [encroached to-expand borders edge-map]}
                (visit-neighbors current center-element new-visited borders
                  edge-map)]
          (if encroached
            {:encroached encroached}
            (recur
              (into (rest expand-queue) to-expand) ; will never have duplicates
              new-visited
              borders
              edge-map))))
      {:encroached nil :visited visited :borders borders :edge-map edge-map})))

(defn- refine-helper [element mesh]
  "Returns `{:n number of inserted elements :bad new bad elements
    :visited old visited elements :borders new border elements}`."
  (dosync
    (let [{:keys [n-refine bad visited borders edge-map]}
            (loop [n-refine 0
                   bad      []
                   visited  []
                   borders  []]
              (if (element/is-garbage? element)
                {:n-refine n-refine :bad bad :visited visited :borders borders}
                (let [res (grow-region element)
                      encroached (:encroached res)]
                  (if encroached
                    (let [_ (element/set-is-referenced? encroached true)
                          {:keys [n bad visited borders]}
                            (refine-helper encroached mesh)]
                      (recur (+ n-refine n) bad visited borders))
                    {:n-refine n-refine
                     :bad      bad
                     :visited  (:visited res)
                     :borders  (:borders res)
                     :edge-map (:edge-map res)}))))
          [n-retriangulate new-bad-elements]
            (if (element/is-garbage? element)
              [0 nil]
              (retriangulate element mesh visited borders edge-map))]
      (doseq [e new-bad-elements]
        (element/set-is-referenced? e true))
      {:n       (+ n-refine n-retriangulate)
       :bad     (into bad (remove element/is-garbage? new-bad-elements))
       :visited visited
       :borders borders})))

(defn refine [element mesh]
  "Refine the region around element, i.e. remove element and replace it with
  something better.

  Returns `{:n number of inserted elements :bad new (non-garbage) bad elements}`."
  (dosync
    (let [{n :n bad :bad} (refine-helper element mesh)]
      (element/set-is-referenced? element false) ; element has been removed
      {:n n :bad bad})))
