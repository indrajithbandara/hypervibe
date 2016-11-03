(ns clojure.stats.distribution.t.critical-value
  (:require [clojure.stats.distribution.t.table :refer [t one-tail-alpha-value two-tail-alpha-value]]))
(use 'clojure.core.matrix)


(defmulti critical-value :tail)

(defmethod critical-value :one [tail]
  (mget t (dec (:dof tail)) ((one-tail-alpha-value) (:alpha tail))))

(defmethod critical-value :two [tail]
  (mget t (dec (:dof tail)) ((two-tail-alpha-value) (:alpha tail))))


