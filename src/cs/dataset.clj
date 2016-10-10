(ns cml.dataset
  (:require [cml.utils :refer [zip]]
            [cml.extract :refer [file-lines]]
            [cml.transform.string :refer [splitter]]))

;TODO Design how data frames will be composable with statistical functions
;TODO Implementparallel data frame
;TODO change to protocols

(defmulti data-frame (fn [type] (:type type)))


(defn- xform-csv
  ([column-names delim]
   (comp (map #(splitter % delim))
         (map #(zip column-names %))))
  ([column-names delim xform]
   (comp (map #(splitter % delim))
         (map #(zip column-names % xform)))))


(defmethod data-frame :csv/read [type]
  (transduce (xform-csv (:column-names type) (:delimiter type) (:xform type))
             conj (:return type)
             (file-lines (:file-path type))))

(defmethod data-frame :csv/write [type] )                   ;TODO will take a data frame and write to csv delimited by ?

