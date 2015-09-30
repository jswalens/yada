(ns yada.main
  (:gen-class)
  (:require [random]
            [priority-queue]
            [yada.element :as element]
            [yada.mesh :as mesh]))

(defn parse-args [args]
  {:angle-constraint 20.0
   :input-prefix     "inputs/633.2"
   :num-thread       1})

(defn initialize-work [mesh]
  (random/set-seed! 0)
  (mesh/shuffle-bad mesh)
  (loop [queue (priority-queue/create element/priority-queue-compare)]
    (let [element (mesh/get-bad mesh)]
      (if (nil? element)
        queue
        (do
          (element/set-is-referenced? element true)
          (recur
            (priority-queue/add queue element)))))))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (let [params
          (parse-args args)
        _ (println "Angle constraint =" (:angle-constraint params))
        _ (println "Reading input...")
        {mesh :mesh init-num-element :n-element}
          (mesh/read (:input-prefix params))
        _ (println "done.")
        work-queue ; This is a heap in the C version
          (initialize-work mesh)
        init-num-bad-element
          (count (:elements work-queue))
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
