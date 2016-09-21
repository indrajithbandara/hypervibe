(ns cml.utils)


(defn zip
  ([keys vals]
   (loop [map (transient {})
          ks (seq keys)
          vs (seq vals)]
     (if (and ks vs)
       (recur (assoc! map (first ks)
                      (first vs))
              (next ks)
              (next vs))
       (persistent! map))))
  ([keys vals xform]
   (if-not (nil? xform)
     (loop [map (transient {})
            ks (seq keys)
            vs (seq vals)]
       (if (and ks vs)
         (recur (assoc! map (first ks)
                        (xform (first vs)))
                (next ks)
                (next vs))
         (persistent! map)))
     (zip keys vals))))


(defn zip-types [keys vals]
  (loop [map (transient {})
         ks (seq keys)
         vs (seq vals)]
    (if (and (apply hash-map
                    ks)
             vs)
      (recur (assoc! map
                     (first ks)
                     (cond (= (second ks) :string)
                           (first vs)
                           (=  (second ks) :integer)
                           (Integer/parseInt (first vs))
                           (= (second ks) :long)
                           (Long/parseLong (first vs))
                           (= (second ks) :double)
                           (Double/parseDouble (first vs))
                           (= (second ks) :character)
                           (.charAt (first vs) 0)
                           :else (first vs)))
             (drop 2 ks)
             (next vs)) (persistent! map))))

