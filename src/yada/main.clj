(ns yada.main
  (:gen-class)
  (:require [priority-queue]
            [yada.options :as options :refer [log error for-all]]
            [yada.element :as element]
            [yada.region :as region]
            [yada.mesh :as mesh]
            [taoensso.timbre.profiling :refer [profile p]]))

(defn- initialize-work [mesh]
  ;(mesh/shuffle-bad mesh) - Don't do this, to get deterministic results
  (let [queue (priority-queue/create element/priority-queue-compare)]
    (loop []
      (if-let [element (mesh/get-bad mesh)]
        (do
          (priority-queue/push queue element)
          (recur))
        queue))))

(defn- process-thread [id mesh work-queue]
  (loop [n-added 0
         i       0]
    ;(mesh/check mesh n-element true)
    (if-let [element (priority-queue/pop work-queue)]               ; atomic
      (do
        (log "Processing element " (element/element->str element))
        (if (element/garbage? element)                              ; atomic
          (recur n-added i)
          (let [{n :n new-bad :bad}
                  (p :refine (region/refine element mesh))]         ; tx
            (log "additional bad elements: " (element/elements->str new-bad))
            (priority-queue/into work-queue new-bad)                ; atomic
            (recur (+ n-added n) (inc i)))))
      {:id id :n-added n-added :n-processed i})))

(defn- process [n-threads mesh work-queue init-n-element]
  (let [threads
          (for-all [i (range n-threads)]
            (future (time (p :process-thread (process-thread i mesh work-queue)))))
        results
          (map deref threads)]
    (for-all [{:keys [id n-added n-processed]} results]
      (println "Thread" id "processed" n-processed "elements."))
    {:n-element
       (+ init-n-element (reduce + (map :n-added results)))
     :n-processed
       (reduce + (map :n-processed results))}))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (let [params (options/set-args args)]
    (profile :trace :all
      (let [_ (println "Reading input...")
            {mesh :mesh init-n-element :n-element}
              (p :read-input (mesh/read (:input params)))
            _ (println "done.")
            work-queue ; This is a heap in the C version
              (p :initialize-work (initialize-work mesh))
            _ (log "Work queue:\n" (element/elements->str (:elements work-queue)))
            init-n-bad-element
              (count (:elements work-queue))
            _ (println "Initial number of mesh elements =" init-n-element)
            _ (println "Initial number of bad elements  =" init-n-bad-element)
            _ (println "Starting triangulation...")
            {final-n-element :n-element n-processed :n-processed}
              (p :process (time
                (process (:thread params) mesh work-queue init-n-element)))
            _ (println "Final mesh size                 =" final-n-element)
            _ (println "Number of elements processed    =" n-processed)
            success?
              (p :check (mesh/check mesh final-n-element false))
            _ (println "Final mesh is" (if success? "valid." "INVALID!"))]
        nil)))
  ; Eliminate one minute wait (see doc shutdown-agents)
  ; shutdown-agents should be after profile, else RejectedExecutionException is
  ; raised in timbre
  (shutdown-agents))
