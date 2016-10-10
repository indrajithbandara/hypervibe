(ns cml.transform.dataframe)

(defn values [m f]
  (reduce-kv (fn [map key val] (assoc map key (f val))) {} m))

(defn by-key
  ([map key transform-fn]
   (assoc map key (transform-fn (get map key))))
  ([map key transform-fn args2]
   (assoc map key (transform-fn (get map key) args2)))
  ([map key transform-fn args2 args3]
   (assoc map key (transform-fn (get map key) args2 args3)))
  ([map key transform-fn args2 args3 args4]
   (assoc map key (transform-fn (get map key) args2 args3 args4)))
  ([map key transform-fn args2 args3 args4 & more]
   (assoc map key (apply transform-fn (get map key) args2 args3 args4 more))))


