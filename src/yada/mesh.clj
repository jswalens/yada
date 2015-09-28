(ns yada.mesh
  (:require [random]))

(defn alloc []
  (ref
    {:root-element   nil
     :init-bad-queue []
     :size 0
     :boundary-set   #{}})) ; TODO: boundary set how?

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
