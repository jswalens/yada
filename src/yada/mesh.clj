(ns yada.mesh
  (:require [clojure.string :as str]
            [random]
            [yada.element :as element]))

(defmacro for-all [seq-exprs body-expr]
  `(doall
    (for ~seq-exprs
      ~body-expr)))

(defn alloc []
  (ref
    {:root-element   nil    ; not a pointer
     :init-bad-queue []
     :size 0
     :boundary-set   #{}})) ; set of boundary edges. TODO: sorted in C version.

(defn insert [mesh element edge-map]
  "Insert `element` in `mesh`. This:
  1. Sets the root element of the mesh if it is empty (only for the first
     insert into a new mesh, or immediately after the previous root has been
     removed).
  2. Checks if the element is really encroached, and updates it if it is not.
  3. If an edge is shared with a previous element in the edge-map, the elements
     become neighbors.
  4. Updates `edge-map` to map record this element's edges.
  Returns the updated `edge-map`."
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
    (doseq [edge (:edges @element)]
      (doseq [neighbor (get edge-map edge [])]
        ; note: there can be at most one neighbor
        (element/add-neighbor element neighbor)
        (element/add-neighbor neighbor element)))
    ; 4. Record this element's edges in edge-map.
    (reduce
      (fn [edge-map edge] (update-in edge-map [edge] conj element))
      edge-map
      (:edges @element))))

(defn remove-element [mesh element]
  "Remove `element` from `mesh`."
  (dosync
    ; If this removes the root, we simply set it to nil. The next mesh/insert
    ; will select a new root, this is OK because every call to mesh/remove is
    ; followed by a call to mesh/insert.
    (when (= (:root-element @mesh) element)
      (alter mesh assoc :root-element nil))
    ; Remove element from neighbors
    (doseq [neighbor (:neighbors @element)]
      (alter neighbor update-in [:neighbors]
        (fn [old-neighbors] (remove #(= element %) old-neighbors))))
    ; Set as garbage
    (element/set-is-garbage? element true)))

(defn insert-boundary [mesh boundary]
  (dosync
    (alter mesh update-in [:boundary-set] conj boundary)))

(defn remove-boundary [mesh boundary]
  (dosync
    (alter mesh update-in [:boundary-set]
      (fn [set] (remove #(= boundary %) set)))))

(defn- create-element [mesh coordinates edge-map]
  "Creates element for `coordinates` and adds it to the `mesh`.
  Returns updates `edge-map`."
  (let [element (element/alloc coordinates)]
    (when (= (count coordinates) 2)
      ; Add to boundary set
      (alter mesh update-in [:boundary-set] conj (element/get-edge element 0)))
    (let [edge-map (insert mesh element edge-map)]
      (when (element/is-bad? element)
        ; Add to initially bad elements
        (alter mesh update-in [:init-bad-queue] conj element))
      edge-map)))

(defn- comment? [line]
  "Line is a comment if it starts with a #"
  (.startsWith line "#"))

(defn- split [line]
  "Split `line` on spaces."
  (str/split line #" "))

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
  (when-not (>= x 0)            (println "ERROR: coordinate should be >= 0"))
  (when-not (<= x n-coordinate) (println "ERROR: coordinate should be <= max")))

(defn- read-node [file-name]
  "Note: the C version numbers coordinates from 1 to n, we go from 0 to n-1."
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                       (line-seq f)
          [n-coordinate n-dimension] (parse-line (first lines) str->int str->int)]
      (when-not (= n-dimension 2)
        (println "ERROR: number of dimensions in node file must be 2-D"))
      (for-all [line (take n-coordinate (rest lines))]
        ; FIXME: (take n-coordinate ...) doesn't work correctly with comments
        (when-not (comment? line)
          (let [[_id x y] (parse-line line str->int str->double str->double)]
            {:x x :y y}))))))

(defn- read-poly [file-name mesh coordinates edge-map]
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                 (line-seq f)
          [n-entry n-dimension] (parse-line (first lines) str->int str->int)
          [n-boundaries]        (parse-line (second lines) str->int)
          n-coordinate          (count coordinates)]
      (when-not (= n-entry 0)
        (println "ERROR: number of entries in poly file must be 0: .node file used for vertices"))
      (when-not (= n-dimension 2)
        (println "ERROR: number of dimensions in poly file must be 2-D: must be edge"))
      {:edge-map
        (reduce
          (fn [edge-map line]
            (if (comment? line)
              edge-map
              (let [[id a b] (parse-line line str->int str->int str->int)]
                (validate-coordinate a n-coordinate)
                (validate-coordinate b n-coordinate)
                (create-element mesh
                  [(nth coordinates (dec a))
                   (nth coordinates (dec b))]
                  edge-map))))
          edge-map
          (take n-boundaries (drop 2 lines))) ; FIXME: doesn't work correctly with comments
       :n-boundaries n-boundaries})))

(defn- read-ele [file-name mesh coordinates edge-map]
  (with-open [f (clojure.java.io/reader file-name)]
    (let [lines                    (line-seq f)
          [n-triangle n-dimension] (parse-line (first lines) str->int str->int)
          n-coordinate             (count coordinates)]
      (when-not (= n-dimension 3)
        (println "ERROR: number of dimensions in ele file must be 3-D: must be triangle"))
      {:edge-map
        (reduce
          (fn [edge-map line]
            (if (comment? line)
              edge-map
              (let [[id a b c] (parse-line line str->int str->int str->int str->int)]
                (validate-coordinate a n-coordinate)
                (validate-coordinate b n-coordinate)
                (validate-coordinate c n-coordinate)
                (create-element mesh
                  [(nth coordinates (dec a))
                   (nth coordinates (dec b))
                   (nth coordinates (dec c))]
                  edge-map))))
          edge-map
          (take n-triangle (rest lines))) ; FIXME: doesn't work correctly with comments
       :n-triangles n-triangle})))

(defn read [file-name-prefix]
  (dosync
    (let [mesh        (alloc)
          edge-map    {}
          ; edge-map maps edges to the elements that contain them.
          ; Each edge is contained by at least one and at most two elements.
          ; In the C version, the edge map either 1) does not contain an edge,
          ; if it has not (yet) been inserted, 2) maps an edge to an element, if
          ; it is contained by one element (up to now), or 3) maps an edge to
          ; nil, if it is contained by two elements (the max, used for detecting
          ; errors later).
          ; In the Clojure version, edge-map maps edges to a list of elements.
          node-file (str file-name-prefix ".node")
          poly-file (str file-name-prefix ".poly")
          ele-file  (str file-name-prefix ".ele")
          ; TODO: check if files exist and abort if not?
          coordinates
            (read-node node-file)
          {:keys [edge-map n-boundaries]}
            (read-poly poly-file mesh coordinates edge-map)
          {:keys [edge-map n-triangles]}
            (read-ele  ele-file  mesh coordinates edge-map)]
      {:mesh      mesh
       :n-element (+ n-boundaries n-triangles)})))

(defn get-bad [mesh]
  "Pop a bad element from the bad queue."
  (dosync
    (let [bad (first (:init-bad-queue @mesh))]
      (when (some? bad)
        (alter mesh update-in [:init-bad-queue] rest))
      bad)))

(defn shuffle-bad [mesh]
  "Shuffle the bad queue."
  (dosync
    (alter mesh update-in [:init-bad-queue] shuffle)))
