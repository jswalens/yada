(ns yada.options
  (:require [clojure.tools.cli]
            [taoensso.timbre :as timbre]))

(def angle-constraint (atom 20.0))

;(defn log [& args] (println (apply str args)))
(defmacro log [& args] nil)

(defn error [& args] (println (apply str "ERROR: " args)))

(defmacro for-all [seq-exprs body-expr]
  `(doall
    (for ~seq-exprs
      ~body-expr)))

(defn reduce-all [f a l]
  (doall (reduce f a l)))

(def cli-params
  [["-a" "--angle FLT"   "Min [a]ngle constraint"
    :default 20.0
    :parse-fn #(Float/parseFloat %)]
   ["-i" "--input STR"   "[i]nput name prefix"
    :default "inputs/spiral.2"]
   ["-t" "--thread UINT" "Number of [t]hreads"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]
   ["-p" "--profile"     "Enable profiling"
    :default false]])

(def usage
  (str
"Usage: ./yada [options]

Options:

" (:summary (clojure.tools.cli/parse-opts nil cli-params))))

(defn set-args [args]
 "Parse, set, and print the command line arguments.

 Ignores errors."
 (let [{:keys [options errors]} (clojure.tools.cli/parse-opts args cli-params)]
   (when (not-empty errors)
     (println "ERROR: Error when parsing command line arguments: "
       errors))

   (when (or (not-empty errors) (:help options))
     (println usage))

   (reset! angle-constraint (:angle options))
   (timbre/set-level! (if (:profile options) :trace :error))

   (println "Min angle constraint =" (:angle options))
   (println "Input name prefix    =" (:input options))
   (println "Number of threads    =" (:thread options))
   (println "Profiling?           =" (:profile options))

   options))
