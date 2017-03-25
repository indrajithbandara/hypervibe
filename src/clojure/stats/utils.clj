(ns clojure.stats.utils
  (:require [clojure.core.reducers :as r]))


(defn ^double mean
  {:doc "Calculates mean"
   :arglists '([data])}
  [data]
  (/ (r/fold + data) (count data)))

(defn ^double mean-1
  {:doc  "Calculates mean -1"
   :arglists '([data])}
  [data]
  (/ (r/fold + data) (dec (count data))))

(defn ^doubles diff
  {:doc "Calculates mean difference"
   :arglists '([data mean])}
  [[sample-one sample-two]] (map - sample-one sample-two))

(defn ssdev [data mean]
  {:doc  "Calculates sample standard deviation"
   :arglists '([data mean])}
  (Math/sqrt (mean-1 (map #(* (- mean %) (- mean %)) data))))

(defn ^double ps-dev
  {:doc "Calculates population standard deviation"
   :arglists '([data mean])}
  [data mean]
  (Math/sqrt (mean (map #(* (- mean %) (- mean %)) data))))

(defn ^double pop-var
  {:doc "Calculates population variance"
   :arglists '([data mean])}
  [data mean]
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (count data)))

(defn ^double svar
  {:doc "Calculates sample variance"
   :arglists '([data mean])}
  [data mean]
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (dec (count data))))

(defn ^double pvar
  {:doc "Calculates pooled variance"
   :arglists '([data mean size-1])}
  [data mean size-1]
  (/ (* size-1 (/ (reduce + (map #(* (- % mean) (- % mean)) data)) size-1)) size-1))

(defn rnull?
  {:doc "Checks whether to reject
         the null hypothesis"
   :arglists '([tstat cval])}
  [tstat cval]
  (> (Math/abs tstat) cval))

(defn oststat
  {:doc "Calculates one sample test
         statistic"
   :arglists '([smpl-mean hmean ssdev ssize])}
  [smpl-mean hmean ssdev ssize]
  (/ (- smpl-mean (.hmean hmean))
     (/ ssdev (Math/sqrt ssize))))

(defn equal-var-tstat
  {:doc "Calculates equal variance test
         statistic"
   :arglists '([sm-one sm-two pmean-one
                pmean-two pv-one pool-var-two
                ssize-one ssize-two])}
  [sm-one sm-two pmean-one
   pmean-two pv-one pool-var-two
   ssize-one ssize-two]
  (/ (- (- sm-one sm-two)
        (- pmean-one pmean-two))
     (Math/sqrt (* (/ (+ pv-one pool-var-two) 2)
                   (+ (/ 1 ssize-one)
                      (/ 1 ssize-two))))))

(defn welch-dof
  {:doc "Calculates Welch's degrees of
         freedom"
   :arglists '([smpl-mean hmean ssdev ssize])}
  [svar-one svar-two ssize-one
   ssize-two]
  (/ (* (+ (/ svar-one ssize-one)
           (/ svar-two ssize-two))
        (+ (/ svar-one ssize-one)
           (/ svar-two ssize-two)))
     (+ (/ (* (/ svar-one ssize-one)
              (/ svar-one ssize-one))
           (- ssize-one 1))
        (/ (* (/ svar-two ssize-two)
              (/ svar-two ssize-two))
           (- ssize-two 1)))))

(defn welch-tstat
  {:doc "Calculates Welch's test
         statistic"
   :arglists '([mean-one mean-two svar-one
                svar-two ssize-one ssize-two])}
  [mean-one mean-two svar-one
   svar-two ssize-one ssize-two]
  (/ (- mean-one mean-two)
     (Math/sqrt (+ (/ svar-one ssize-one)
                   (/ svar-two ssize-two)))))

(defn rmsure-tstat
  {:doc "Calculates repeated measure test
         statistic"
   :arglists '([dmean pmean-one pmean-two
                sdev ssize])}
  [dmean pmean-one pmean-two
   sdev ssize]
  (/ (- dmean
        (- pmean-one pmean-two))
     (/ sdev
        (Math/sqrt ssize))))

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


