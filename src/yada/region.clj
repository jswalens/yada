(ns yada.region
  (:require [priority-queue]
            [yada.element :as element]
            [yada.mesh :as mesh]))

;(def log println)
(defn log [& _] nil)

(defn alloc []
  (ref ; XXX one ref?
    {:center-coordinate nil
     ;:expand-queue      (list) -> only used in grow-region, make it local there
     ;:before-list       [] ; before retriangulation; list to avoid duplicates; XXX sort using element_listCompare
     ;:border-list       [] ; edges adjacent to region; list to avoid duplicates; XXX sort uning element_listCompareEdge
     :bad-vector        []})) ; list of bad elements

(defn add-to-bad-vector [region bad-element]
  (dosync
    (alter region update-in [:bad-vector] conj bad-element)
    (element/set-is-referenced? bad-element true)))

(defn retriangulate [element mesh visited borders edge-map]
  "Returns [n-inserted new-bad-elements]."
  (let [center-coordinate
          (element/get-new-point element)
        _ ; Remove the old triangles
          (doseq [v visited]
            (log "Removing" (element/element->str v))
            (mesh/remove-element mesh v))
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
          (doall
            (reduce
              (fn [edge-map after]
                (log "Inserting" (element/element->str after))
                (mesh/insert-element mesh after edge-map))
              edge-map
              after-elements))
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

(defn- visit-neighbors [current center-element visited borders edge-map]
  (let [neighbors         (:neighbors @current)
        boundary?         (= (element/get-num-edge center-element) 1)
        center-coordinate (element/get-new-point center-element)]
    (reduce
      (fn [{:keys [encroached to-expand borders edge-map] :as m} neighbor]
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
            (-> m
              (update-in [:borders] conj border-edge)
              (update-in [:edge-map border-edge] conj neighbor)))))
      {:encroached nil :to-expand [] :borders borders :edge-map edge-map}
      (remove #(.contains visited %) neighbors))))

(defn grow-region [center-element]
  "Returns either `{:encroached encroached-neighbor}` or
  `{:encroached nil :visited visited :borders borders :edge-map edge-map}`."
  (loop [expand-queue [center-element]
         visited      []
         borders      []
         edge-map     {}]
    (if-let [current (first expand-queue)]
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
            edge-map)))
      {:encroached nil :visited visited :borders borders :edge-map edge-map})))

(defn refine [region element mesh]
  "Returns {:n number of inserted elements :visited visited :borders borders}."
  (dosync
    (let [{:keys [n-refine visited borders edge-map]}
            (loop [n-refine 0
                   visited  []
                   borders  []]
              (if (element/is-garbage? element)
                {:n-refine n-refine :visited visited :borders borders}
                (let [res (grow-region element)
                      encroached (:encroached res)]
                  (if encroached
                    (let [_ (element/set-is-referenced? encroached true)
                          {:keys [n visited borders]}
                            (refine region encroached mesh)]
                      (recur (+ n-refine n) visited borders))
                    {:n-refine n-refine
                     :visited  (:visited res)
                     :borders  (:borders res)
                     :edge-map (:edge-map res)}))))
          [n-retriangulate new-bad-elements]
            (if (element/is-garbage? element)
              [0 nil]
              (retriangulate element mesh visited borders edge-map))]
      (alter region update-in [:bad-vector] into new-bad-elements)
      {:n (+ n-refine n-retriangulate) :visited visited :borders borders})))

(defn clear-bad [region]
  (alter region assoc :bad-vector nil))

(defn transfer-bad [region work-queue]
  "Add all non-garbage bad elements from `region` to `work-queue`."
  (dosync
    (doseq [e (remove element/is-garbage? (:bad-vector @region))]
      (priority-queue/push work-queue e))))
