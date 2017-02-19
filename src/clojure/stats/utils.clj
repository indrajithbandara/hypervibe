(ns clojure.stats.utils
  (:require [clojure.core.reducers :as r]))


(defn ^double mean [data] (/ (r/fold + data) (count data)))

(defn ^double mean-1 [data] (/ (r/fold + data) (dec (count data))))

(defn ^doubles diff [[sample-one sample-two]] (map - sample-one sample-two))

(defn permutations
  [x xs]
  (letfn [(factorial [x]
            (loop [cnt (if (coll? x)
                         (count x) x) acc 1]
              (if (zero? cnt)
                acc (recur (dec cnt) (*' cnt acc)))))]
    (quot (factorial x)
          (factorial (- x xs)))))

(defn pplus ^double [^double x ^double y] (+ x y))

(defn ptimes ^double [^double x ^double y] (* x y))

(defn pdiv ^double [^double x ^double y] (/ x y))

(defn pminus ^double [^double x ^double y] (- x y))

(defn sqr ^double [^double x] (* x x))

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
                           (= (second ks) :integer)
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


(defn ssdev [data mean]
  "Computes the sample standard deviation"
  (Math/sqrt (mean-1 (map #(* (- mean %)
                              (- mean %)) data))))

(defn ps-dev [data mean]
  "Computes the population standard deviation"
  (Math/sqrt (mean (map #(* (- mean %) (- mean %)) data))))


(defn pop-var [data mean]
  "Computes the population variance"
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (count data)))


(defn svar [data mean]
  "Computes the sample variance"
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (dec (count data))))


(defn pvar [data mean size-minus-one]
  "Computes the pooled variance"
  (/ (* size-minus-one
        (/ (reduce + (map #(* (- % mean) (- % mean)) data))
           size-minus-one)) size-minus-one))


(defn rnull?
  [tstat cval]
  (> (Math/abs tstat) cval))


(defn oststat
  [smpl-mean hmean ssdev smpl-size]
  (/ (- smpl-mean (.hmean hmean))
     (/ ssdev (Math/sqrt smpl-size))))


(defn equal-var-tstat
  [smpl-mean-one smpl-mean-two pmean-one
   pmean-two pool-var-one pool-var-two
   smpl-size-one smpl-size-two]
  (/ (- (- smpl-mean-one smpl-mean-two)
        (- pmean-one pmean-two))
     (Math/sqrt (* (/ (+ pool-var-one pool-var-two) 2)
                   (+ (/ 1 smpl-size-one)
                      (/ 1 smpl-size-two))))))


(defn welch-dof
  [svar-one svar-two smpl-size-one
   smpl-size-two]
  (/ (* (+ (/ svar-one smpl-size-one)
           (/ svar-two smpl-size-two))
        (+ (/ svar-one smpl-size-one)
           (/ svar-two smpl-size-two)))
     (+ (/ (* (/ svar-one smpl-size-one)
              (/ svar-one smpl-size-one))
           (- smpl-size-one 1))
        (/ (* (/ svar-two smpl-size-two)
              (/ svar-two smpl-size-two))
           (- smpl-size-two 1)))))


(defn welch-tstat
  [mean-one mean-two svar-one
   svar-two smpl-size-one smpl-size-two]
  (/ (- mean-one mean-two)
     (Math/sqrt (+ (/ svar-one smpl-size-one)
                   (/ svar-two smpl-size-two)))))


(defn rmsure-tstat
  [diff-mean pmean-one pmean-two
   sdev smpl-size]
  (/ (- diff-mean
        (- pmean-one pmean-two))
     (/ sdev
        (Math/sqrt smpl-size))))

