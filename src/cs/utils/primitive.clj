(ns cs.utils.primitive)

(defn pplus ^double [^double x ^double y]
  (+ x y))

(defn ptimes ^double [^double x ^double y]
  (* x y))

(defn pdiv ^double [^double x ^double y]
  (/ x y))

(defn pminus ^double [^double x ^double y]
  (- x y))

(defn sqr ^double [^double x]
  (* x x))

