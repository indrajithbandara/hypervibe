(ns cml.utils.vector
  (:require [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]))

(defn vminus [vec-one vec-two]
  "Subtracts two vectors"
  (neanderthal/axpy -1 vec-one vec-two))

(defn vplus [vec-one vec-two]
  "Adds two vectors"
  (neanderthal/axpy 1 vec-one vec-two))

