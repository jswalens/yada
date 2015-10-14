(ns yada.main
  (:gen-class)
  (:require [priority-queue]
            [yada.options :as options :refer [log error]]
            [yada.element :as element]
            [yada.region :as region]
            [yada.mesh :as mesh]))

(defn- initialize-work [mesh]
  ;(mesh/shuffle-bad mesh) - Don't do this, to get deterministic results
  (let [queue (priority-queue/create element/priority-queue-compare)]
    (loop []
      (if-let [element (mesh/get-bad mesh)]
        (do
          (element/set-is-referenced? element true) ; TODO: why?
          (priority-queue/push queue element)
          (recur))
        queue))))

(defn- process [mesh init-n-element work-queue]
  (loop [n-element init-n-element
         i         0]
    ;(mesh/check mesh n-element true)
    (if-let [element (priority-queue/pop work-queue)]
      (do
        (log "Processing element " (element/element->str element))
        (if (element/is-garbage? element)
          (recur n-element i)
          (let [{n-added :n new-bad-elements :bad} (region/refine element mesh)]
            (log "additional bad elements: "
              (element/elements->str new-bad-elements))
            (priority-queue/into work-queue new-bad-elements)
            (recur (+ n-element n-added) (inc i)))))
      {:n-element n-element :n-processed i})))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (let [params
          (options/set-args args)
        _ (println "Reading input...")
        {mesh :mesh init-num-element :n-element}
          (mesh/read (:input params))
        _ (println "done.")
        work-queue ; This is a heap in the C version
          (initialize-work mesh)
        _ (log "Work queue:\n" (element/elements->str (:elements @work-queue)))
        init-num-bad-element
          (count (:elements @work-queue))
        _ (println "Initial number of mesh elements =" init-num-element)
        _ (println "Initial number of bad elements  =" init-num-bad-element)
        _ (println "Starting triangulation...")
        {final-n-element :n-element n-processed :n-processed}
          (time (process mesh init-num-element work-queue))
          ; TODO: (time (thread/start (process mesh init-num-element work-queue)))
        _ (println "Final mesh size                 =" final-n-element)
        _ (println "Number of elements processed    =" n-processed)
        success?
          (mesh/check mesh final-n-element false)
        _ (println "Final mesh is" (if success? "valid." "INVALID!"))]
    nil))
