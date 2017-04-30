(ns clojure.stats.utils
    (:require [clojure.core.reducers :as r]
              [clojure.core.matrix.operators :as op]
              [clojure.core.matrix :as matrix]))
(matrix/set-current-implementation :vectorz)

(defn ^double -mean
    {:doc      "Mean"
     :arglists '([^mikera.vectorz.Vector data])}
    [data]
    (op/div= (reduce matrix/add data)) (matrix/ecount data))

(defn ^double mean
    {:doc "Mean"
     :arglists '([data])}
    [data]
    (/ (r/fold + data) (count data)))

(defn ^double mean-1
    {:doc      "Mean -1"
     :arglists '([data])}
    [data]
    (/ (r/fold + data) (dec (count data))))

(defn ^doubles diff
    {:doc      "Mean difference"
     :arglists '([data mean])}
    [[sample-one sample-two]] (map - sample-one sample-two))

(defn ssdev [data mean]
    {:doc      "Sample standard deviation"
     :arglists '([data mean])}
    (Math/sqrt (mean-1 (map #(* (- mean %) (- mean %)) data))))

(defn ^double ps-dev
    {:doc      "Population standard deviation"
     :arglists '([data mean])}
    [data mean]
    (Math/sqrt (mean (map #(* (- mean %) (- mean %)) data))))

(defn ^double pop-var
    {:doc      "Population variance"
     :arglists '([data mean])}
    [data mean]
    (/ (reduce + (map #(* (- % mean) (- % mean)) data))
       (count data)))

(defn ^double svar
    {:doc      "Sample variance"
     :arglists '([data mean])}
    [data mean]
    (/ (reduce + (map #(* (- % mean) (- % mean)) data))
       (dec (count data))))

(defn ^double pvar
    {:doc      "Pooled variance"
     :arglists '([data mean size-1])}
    [data mean size-1]
    (/ (* size-1 (/ (reduce + (map #(* (- % mean) (- % mean)) data)) size-1)) size-1))

(defn rnull?
    {:doc      "Reject the null hypothesis?"
     :arglists '([tstat cval])}
    [tstat cval]
    (> (Math/abs tstat) cval))

(defn oststat
    {:doc      "One sample test statistic"
     :arglists '([smpl-mean hmean ssdev ssize])}
    [smpl-mean hmean ssdev ssize]
    (/ (- smpl-mean (.hmean hmean))
       (/ ssdev (Math/sqrt ssize))))

(defn equal-var-tstat
    {:doc      "Equal variance test statistic"
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
    {:doc      "Welch's degrees of freedom"
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
    {:doc      "Welch's test statistic"
     :arglists '([mean-one mean-two svar-one
                  svar-two ssize-one ssize-two])}
    [mean-one mean-two svar-one
     svar-two ssize-one ssize-two]
    (/ (- mean-one mean-two)
       (Math/sqrt (+ (/ svar-one ssize-one)
                     (/ svar-two ssize-two)))))

(defn rmsure-tstat
    {:doc      "Repeated measure test
         statistic"
     :arglists '([dmean pmean-one pmean-two
                  sdev ssize])}
    [dmean pmean-one pmean-two
     sdev ssize]
    (/ (- dmean
          (- pmean-one pmean-two))
       (/ sdev
          (Math/sqrt ssize))))

(defn gamma
    {:doc      "Gamma function using lanczos approximation"
     :arglists '([x])}
    [x]
    (if (< x 0.5)
        (/ Math/PI (* (Math/sin (* Math/PI x))
                      (gamma (- 1 x))))
        (let [n (dec x)
              c [0.99999999999980993 676.5203681218851 -1259.1392167224028
                 771.32342877765313 -176.61502916214059 12.507343278686905
                 -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]]
            (* (Math/sqrt (* 2 Math/PI))
               (Math/pow (+ n 7 0.5) (+ n 0.5))
               (Math/exp (- (+ n 7 0.5)))
               (+ (first c)
                  (apply + (map-indexed #(/ %2 (+ n %1 1)) (next c))))))))

(defn zip-types [keys vals]
    {:doc      "Zipmap with type parsing"
     :arglists '([keys vals])}
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


