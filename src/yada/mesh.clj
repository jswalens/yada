(ns yada.mesh
  (:refer-clojure :exclude [read])
  (:require [clojure.string]
            [yada.options :as options :refer [log error for-all]]
            [yada.element :as element]
            [taoensso.timbre.profiling :refer [p]])
  (:import [java.util ArrayList]))

(defn alloc []
  (ref
    {:root-element   nil    ; not a pointer
     :edge-map       {}
     ; edge-map maps edges to the elements that contain them.
     ; Each edge is contained by at least one and at most two elements.
     ; In the C version, the edge map either 1) does not contain an edge, if it
     ; has not (yet) been inserted, 2) maps an edge to an element, if it is
     ; contained by one element (up to now), or 3) maps an edge to nil, if it is
     ; contained by two elements (the max, used for detecting errors later).
     ; In the Clojure version however, edge-map maps edges to a set of elements.
     ; Also, in the C version the edge map is created when needed, while in the
     ; Clojure version it is a permanent part of the mesh.
     :boundary-set   #{}})) ; set of boundary edges

(defn- edge-map-get [mesh edge]
  "Find `edge` in the edge-map of `mesh`."
  (get-in @mesh [:edge-map edge] #{}))

(defn- edge-map-put [mesh edge element]
  "Say that `element` has `edge` in the edge-map of `mesh`. Checks whether
  there's at most two elements per edge: there cannot be more than two triangles
  that share an edge."
  (dosync
    (let [existing (edge-map-get mesh edge)]
      (when-not (contains? existing element)
        (if (<= (count existing) 2)
          ; first two unique elements: OK
          (alter mesh assoc-in [:edge-map edge] (conj existing element))
          ; edge cannot be shared by more than two distinct elements
          (error "more than two distinct elements for edge " edge
            " in edge-map! It contains: "
            (element/elements->str (edge-map-get mesh edge))
            "; and we're adding " (element/element->str element)))))))

(defn edge-map-put-if-empty [mesh edge element]
  "Say that `element` has `edge` in the edge-map of `mesh`; but only if there's
  no element for `edge` yet."
  (dosync
    (when (empty? (edge-map-get mesh edge))
      (print "æ")
      (alter mesh assoc-in [:edge-map edge] #{element}))))

(defn- edge-map-put-element [mesh element]
  "Put the edges of `element` in the edge-map of `mesh`."
  (doseq [edge (:edges @element)]
    (edge-map-put mesh edge element)))

(defn- edge-map-remove-element [mesh element]
  "Remove `element` from the edge-map of `mesh`."
  (dosync
    (doseq [edge (:edges @element)]
      (alter mesh update-in [:edge-map edge] disj element))))

(defn insert-element [mesh element]
  "Insert `element` in `mesh`. This:
  1. Sets the root element of the mesh if it is empty (only for the first
     insert into a new mesh, or immediately after the previous root has been
     removed).
  2. Checks if the element is really encroached, and updates it if it is not.
  3. If an edge is shared with a previous element in the edge-map, the elements
     become neighbors.
  4. Updates edge-map of `mesh` to map record this element's edges."
  (log "Inserting " (element/element->str element))
  (dosync
    ; 1. We assume the mesh is a fully connected graph, so we just need the
    ; pointer to one element (the "root") and we can reach all others. If there
    ; is no root element yet, make this one the root.
    ; The root element is not needed for the actual refining, but rather for
    ; checking the validity of the final mesh.
    (when (nil? (:root-element @mesh))
      (alter mesh assoc :root-element element))
    ; 2. Check if really encroached: if the element has an encroached edge, but
    ; this edge does not appear in the boundary set of the mesh, it is not
    ; really encroached and should be removed in the element.
    (when-let [encroached-i (:encroached-edge @element)]
      (when-not (.contains (:boundary-set @mesh) (element/get-edge element encroached-i))
        (element/clear-encroached element)))
    ; 3. Search edge-map for neighbors
    (doseq [edge (:edges @element)
            :let [neighbors (edge-map-get mesh edge)]]
      ; note: there can be at most one neighbor along an edge
      (when (> (count neighbors) 1)
        (error "element " (element/element->str element) " should have at most "
          "one neighbor along edge " edge ", but it has " (count neighbors) ": "
          (element/elements->str neighbors)))
      (doseq [neighbor neighbors]
        (log "Neighbor: " (element/element->str neighbor))
        (element/add-neighbor element neighbor)
        (element/add-neighbor neighbor element)))
    ; 4. Record this element's edges in edge-map.
    (edge-map-put-element mesh element)
    ; Return element
    element))

(defn remove-element [mesh element]
  "Remove `element` from `mesh`."
  (log "Removing " (element/element->str element))
  (dosync
    ; If this removes the root, we simply set it to nil. The next
    ; mesh/insert-element will select a new root, this is OK because every call
    ; to mesh/remove-element is followed by a call to mesh/insert-element.
    (when (= (:root-element @mesh) element)
      (alter mesh assoc :root-element nil))
    ; Remove element from neighbors
    (doseq [neighbor (:neighbors @element)]
      (element/remove-neighbor element neighbor)
      (element/remove-neighbor neighbor element))
    ; Remove element from edge-map
    (edge-map-remove-element mesh element)
    ; Set as garbage
    (element/set-garbage element true)))

(defn insert-boundary [mesh boundary]
  (dosync
    (alter mesh update-in [:boundary-set] conj boundary)))

(defn remove-boundary [mesh boundary]
  (dosync
    (alter mesh update-in [:boundary-set] disj boundary)))

(defn- comment? [line]
  "Line is a comment if it starts with a #"
  (.startsWith line "#"))

(defn- split [line]
  "Split `line` on spaces."
  (clojure.string/split line #" "))

(defn- str->int [s]
  (Integer/parseInt s))

(defn- str->double [s]
  (Double/parseDouble s))

(defn- parse-line [line & conversion-fns]
  "Split `line` on spaces, and apply the i'th `conversion-fn` to each word `i`.
  E.g. (parse-line '1 2.3' str->int str->double) = '(1 2.3)."
  (->> line
    (split)
    (remove empty?)
    (take (count conversion-fns))
    (map-indexed (fn [i w] ((nth conversion-fns i) w)))))

(defn- validate-coordinate [x n-coordinate]
  (when-not (>= x 0)            (error "coordinate should be >= 0"))
  (when-not (<= x n-coordinate) (error "coordinate should be <= max")))

(defn- read-node [file-name]
  "Note: the C version numbers coordinates from 1 to n, we go from 0 to n-1."
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                       (line-seq f)
          [n-coordinate n-dimension] (parse-line (first lines) str->int str->int)
          coordinates (ArrayList. n-coordinate)]
      (when-not (= n-dimension 2)
        (error "number of dimensions in node file must be 2-D"))
      (doseq [line (take n-coordinate (rest lines))]
        ; FIXME: (take n-coordinate ...) doesn't work correctly with comments
        (when-not (comment? line)
          (let [[_id x y] (parse-line line str->int str->double str->double)]
            (.add coordinates {:x x :y y}))))
      coordinates)))

(defn- create-element [mesh coordinates]
  "Creates element for `coordinates` and adds it to the `mesh`."
  (let [element (element/alloc coordinates)]
    (when (= (count coordinates) 2)
      ; Add to boundary set
      (alter mesh update-in [:boundary-set] conj (element/get-edge element 0)))
    (insert-element mesh element)
    ; Return element
    element))

(defn- read-poly [file-name mesh coordinates]
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                 (line-seq f)
          [n-entry n-dimension] (parse-line (first lines) str->int str->int)
          [n-boundaries]        (parse-line (second lines) str->int)
          n-coordinate          (count coordinates)
          elements
            (for-all [line (take n-boundaries (drop 2 lines))]
              (when-not (comment? line)
                (let [[id a b] (parse-line line str->int str->int str->int)]
                  (validate-coordinate a n-coordinate)
                  (validate-coordinate b n-coordinate)
                  (create-element mesh
                    [(nth coordinates (dec a))
                     (nth coordinates (dec b))]))))
          bad-elements
            (filter element/bad? elements)]
      (when-not (= n-entry 0)
        (error "number of entries in poly file must be 0: .node file used for vertices"))
      (when-not (= n-dimension 2)
        (error "number of dimensions in poly file must be 2-D: must be edge"))
      {:n n-boundaries :bad bad-elements})))

(defn- read-ele [file-name mesh coordinates]
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                    (line-seq f)
          [n-triangle n-dimension] (parse-line (first lines) str->int str->int)
          n-coordinate             (count coordinates)
          elements
            (for-all [line (take n-triangle (rest lines))]
              (when-not (comment? line)
                (let [[id a b c] (parse-line line str->int str->int str->int str->int)]
                  (validate-coordinate a n-coordinate)
                  (validate-coordinate b n-coordinate)
                  (validate-coordinate c n-coordinate)
                  (create-element mesh
                    [(nth coordinates (dec a))
                     (nth coordinates (dec b))
                     (nth coordinates (dec c))]))))
          bad-elements
            (filter element/bad? elements)]
      (when-not (= n-dimension 3)
        (error "number of dimensions in ele file must be 3-D: must be triangle"))
      {:n n-triangle :bad bad-elements})))

(defn read [file-name-prefix]
  (dosync
    (let [mesh        (alloc)
          node-file (str file-name-prefix ".node")
          poly-file (str file-name-prefix ".poly")
          ele-file  (str file-name-prefix ".ele")
          ; TODO: check if files exist and abort if not?
          coordinates
            (p :read-node (read-node node-file))
          {n-boundaries :n bad-boundaries :bad}
            (p :read-poly (read-poly poly-file mesh coordinates))
          {n-triangles  :n bad-triangles  :bad}
            (p :read-ele  (read-ele  ele-file  mesh coordinates))]
      {:mesh mesh
       :n    (+ n-boundaries n-triangles)
       :bad  (into bad-triangles bad-boundaries)})))

(defn check [mesh expected-n-element ongoing?]
  (when (nil? (:root-element @mesh))
    (error "root element should not be nil"))
  ; Breadth-first search
  (let [[n-element n-bad-triangle]
          (loop [queue          [(:root-element @mesh)]
                 visited        #{}
                 n-element      0
                 n-bad-triangle 0]
            (let [[cur & rst] queue]
              (if (nil? cur)
                [n-element n-bad-triangle]
                (if (contains? visited cur)
                  (recur rst visited n-element n-bad-triangle)
                  (let [new-visited
                          (conj visited cur)
                        new-queue
                          (into rst (remove #(contains? new-visited %) (:neighbors @cur)))
                        new-n-bad-triangle
                          (if (element/check-angles cur)
                            n-bad-triangle
                            (+ n-bad-triangle 1))]
                    (recur
                      new-queue
                      new-visited
                      (+ n-element 1)
                      new-n-bad-triangle))))))]
    (println "Number of elements      =" n-element)
    (println "Number of bad triangles =" n-bad-triangle)
    (when (not= n-element expected-n-element)
      (error "number of elements actually in mesh (" n-element
        ") != number of elements reportedly in mesh (" expected-n-element ")"))
    (when (and (not ongoing?) (> n-bad-triangle 0))
      (error "number of bad triangles should be 0"))
    (and (some? (:root-element @mesh))
         (= n-element expected-n-element)
         (or ongoing? (= n-bad-triangle 0)))))
