(ns hypervibe.core.api.samples
    (:require [clojure.core.matrix.operators :as op]
              [clojure.core.matrix :as m]))
(m/set-current-implementation :vectorz)

(def population-one (m/array [490 500 530 550 580 590 600 600 650 700]))
(def ballet-dancers [89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3])
(def football-players [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9])
(def before [220 240 225 180 210 190 195 200 210 240])
(def after [200 210 210 170 220 180 190 190 220 210])
