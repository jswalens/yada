(ns yada.options)

(def angle-constraint 20.0)

;(defn log [& args] (println (apply str args)))
(defn log [& args] nil)

(defn error [& args] (apply println "ERROR:" args))
