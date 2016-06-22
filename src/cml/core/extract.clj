(ns cml.core.extract)


(defn file-lines [^String file-path]
  (letfn [(helper [rdr]
            (lazy-seq (if-let [line (.readLine rdr)]
                        (cons line
                              (helper rdr))
                        (do (.close rdr) nil))))]
    (helper (clojure.java.io/reader file-path))))

;TODO add functions for extracting data from different databases, files, databases etc


