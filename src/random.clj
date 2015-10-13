(ns random
  (:refer-clojure :exclude [rand-int]))

(def rng (new java.util.Random 1))

(defn set-seed! [seed]
  "Set seed of random number generator."
  (.setSeed rng seed))

(defn rand-int [n]
  "Get a pseudorandom int between 0 (inclusive) and `n` (exclusive)."
  (.nextInt rng n))
