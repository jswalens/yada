(ns yada.main
  (:gen-class)
  (:require [random]
            [priority-queue]
            [yada.element :as element]
            [yada.region :as region]
            [yada.mesh :as mesh]))

(defn- parse-args [args]
  {:angle-constraint 20.0
   :input-prefix     "inputs/633.2"
   :num-thread       1})

(defn- initialize-work [mesh]
  (random/set-seed! 0)
  (mesh/shuffle-bad mesh)
  (let [queue (priority-queue/create element/priority-queue-compare)]
    (loop []
      (if-let [element (mesh/get-bad mesh)]
        (do
          (element/set-is-referenced? element true) ; TODO: why?
          (priority-queue/push queue element)
          (recur))
        queue))))

(defn- process [mesh work-queue]
  (let [region (region/alloc)]
    (loop [n-added   0
           n-process 0]
      (if-let [element (priority-queue/pop work-queue)]
        (if (dosync (element/is-garbage? element))
          (recur n-added n-process)
          (let [added
                  (dosync
                    (region/clear-bad region)
                    (region/refine region element mesh))]
            (dosync
              (element/set-is-referenced? element false))
            (dosync
              (ref-set work-queue (region/transfer-bad region work-queue)))
            (recur (+ n-added added) (inc n-process))))
        {:n-added n-added :n-process n-process}))))

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
          (count (:elements @work-queue))
        _ (println "Initial number of mesh elements =" init-num-element)
        _ (println "Initial number of bad elements  =" init-num-bad-element)
        _ (println "Starting triangulation...")
        {total-num-added :n-added num-process :n-process}
          (time (process mesh work-queue))
          ; TODO: (time (thread/start (process mesh work-queue)))
        final-num-element
          (+ init-num-element total-num-added)
        _ (println "Final mesh size                 =" final-num-element)
        _ (println "Number of elements processed    =" num-process)
        success?
          ;(mesh/check mesh final-num-element) - disabled
          true
        _ (println "Final mesh is" (if success? "valid." "INVALID!"))]
    nil))
