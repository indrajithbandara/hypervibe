(ns hypervibe.core.api.utils
	(:require [clojure.core.reducers :as r]
			  [clojure.core.matrix :as m]
			  [clojure.core.matrix.operators :as op]
			  [criterium.core :as cri])
	(:import (mikera.vectorz Vector)))
(m/set-current-implementation :vectorz)


(defn ^double mean
	{:doc      "Mean"
	 :arglists '([mikera.vectorz.Vector data])}
	[^Vector data]
	(m/div (m/esum data)
		   (m/ecount data)))

(defn ^double mean-1
	{:doc      "Mean -1"
	 :arglists '([mikera.vectorz.Vector data])}
	[data]
	(m/div (m/esum data)
		   (dec (m/ecount data))))

(defn ^doubles diff
	{:doc      "Mean difference"
	 :arglists '([mikera.vectorz.Vector data pmean])}
	[[data-one data-two]]
	(map op/- data-one
		 data-two))


(defn ^double ssdev [data mean]
	{:doc      "Sample standard deviation"
	 :arglists '([data mean])}
	(Math/sqrt (mean-1 (map #(* (- mean %)
								(- mean %))
							data))))

(defn ^double psdev
	{:doc      "Population standard deviation"
	 :arglists '([data pmean])}
	[data pmean]
	(Math/sqrt (pmean (map #(* (- pmean %)
							   (- pmean %))
						   data))))

(defn ^double popvar
	{:doc      "Population variance"
	 :arglists '([data pmean])}
	[data pmean]
	(/ (reduce +
			   (map #(* (- % pmean)
						(- % pmean))
					data))
	   (count data)))

(defn ^double svar
	{:doc      "Sample variance"
	 :arglists '([data pmean])}
	[data pmean]
	(/ (reduce +
			   (map #(* (- % pmean)
						(- % pmean))
					data))
	   (dec (count data))))

(defn ^double pvar
	{:doc      "Pooled variance"
	 :arglists '([data pmean size-1])}
	[data pmean size-1]
	(/ (* size-1
		  (/ (reduce +
					 (map #(* (- % pmean)
							  (- % pmean))
						  data))
			 size-1))
	   size-1))

(defn rnull?
	{:doc      "Reject the null hypothesis?"
	 :arglists '([tstat cval])}
	[tstat cval]
	(> (Math/abs tstat)
	   cval))

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
		   ks  (seq keys)
		   vs  (seq vals)]
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


