(ns cml.transform.string
  (:import (com.google.common.base Splitter)))

(defn splitter
  ([^String line] (splitter line \,))
  ([^String line ^CharSequence delim]
   (iterator-seq (.iterator (.split (Splitter/on delim) line)))))

(defn splitter-on-pattern
  ([^String line]
   (iterator-seq (.iterator (.split (Splitter/onPattern ",") line))))
  ([^String line ^String delim]
   (iterator-seq (.iterator (.split (Splitter/onPattern delim) line)))))

(defn splitter-fixed-length
  ([^String line]
   (iterator-seq (.iterator (.split (Splitter/fixedLength 1) line))))
  ([^String line ^Long delim]
   (iterator-seq (.iterator (.split (Splitter/fixedLength delim) line)))))


