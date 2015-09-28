(ns yada.main
  (:gen-class))

(defn parse-args [args]
  {:angle-constraint 20.0
   :input-prefix     "inputs/633.2"
   :num-thread       1})

(defn initialize-work [heap mesh]
  0) ; TODO

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (let [params
          (parse-args args)
        mesh
          ; TODO: (mesh/alloc)
          nil
        _ (println "Angle constraint =" (:angle-constraint params))
        _ (println "Reading input...")
        init-num-element
          ; TODO: (mesh/read mesh (:input-prefix params))
          0
        _ (println "done.")
        work-heap
          ; TODO: (heap/alloc 1 element/heap-compare)
          nil
        init-num-bad-element
          (initialize-work work-heap mesh)
        _ (println "Initial number of mesh elements =" init-num-element)
        _ (println "Initial number of bad elements  =" init-num-bad-element)
        _ (println "Starting triangulation...")
        [total-num-added num-process]
          ; TODO: (time (thread/start process))
          (time [0 0])
        final-num-element
          (+ init-num-element total-num-added)
        _ (println "Final mesh size                 =" final-num-element)
        _ (println "Number of elements processed    =" num-process)
        success?
          ;(mesh/check mesh final-num-element) - disabled
          true
        _ (println "Final mesh is" (if success? "valid." "INVALID!"))]
    nil))
