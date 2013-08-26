(ns matrixtests-test
  (:require [clojure.test :refer :all]
            [matrixtests :refer :all]
            [criterium.core :as crit]
            [clatrix.core :as clx]
            [clojure.core.matrix :as M])
  (:use midje.sweet)
  (:import [org.jblas DoubleMatrix]))


(set! *warn-on-reflection* true)


(defmacro msecs
  "Like clojure.core/time, returns the time as a value
   instead of a string."
  [expr]
  `(let [start# (. System (nanoTime))]
     (do
       ~expr
       (/ (double (- (. System (nanoTime)) start#)) 1000000.0))))

(defmacro measure [expr]
  `(do
     (println (str '~expr))
     (println ": " (msecs ~expr))))


(def dd (make-array Double/TYPE 1000 1000))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))


(fact "aget! variants" :aget

;; too slow without hints
;;
;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (aget dd i j))))
;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (-> dd (aget i) (aget j)))))


(println "- optimal hints on aget, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (let [#^doubles a (aget #^objects dd i)] (aget a j)))))


(println "- optimal hints/macros on aget, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (deep-aget doubles dd i j))))


(println "- optimal hints/macros/cg-aget! on aget, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (cg-aget! dd i j))))


(println "- optimal hints/macros/f-cg-aget! on aget, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (f-cg-aget! dd i j))))


(println "- optimal hints/macros/cg-aget! on aget, LJensen")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (lj-aget! dd i j))))

true => truthy)

(fact "aset! variants" :aset

;; too slow without hints
;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (aset dd i j 42.0))))

;; still too slow
;;
;;(println "- hints on aset, CGrande")
;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (-> #^objects dd (aget i) (aset j 42.0)))))


(println "- improved hints on aset, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))))


(println "- optimal hints on aset, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))))


(println "- optimal hints/macros on aset, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (deep-aset doubles dd i j 42.0))))


(println "- optimal hints/macros/cg-aset! on aset, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (cg-aset! dd i j 42.0))))


(println "- optimal hints/macros/f-cg-aset! on aset, CGrande")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (f-cg-aset! dd i j 42.0))))


(println "- optimal hints/macros/jl-aset! on aset, LJensen")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (lj-aset! dd i j 42.0))))

true => truthy)


(fact "Clatrix" :clx

(println "- Clatrix get")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (clx/get DD i j))))

(println "- Clatrix set")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (clx/set DD i j 42.0))))

true => truthy)


(fact "core.matrix" :mikera

(println "- core.matrix mget")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (M/mget DCM i j))))

(println "- core.matrix mset")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (M/mset DCM i j 42.0))))

(println "- core.matrix mset")
(crit/bench (dotimes [i 1000] (dotimes [j 1000] (M/mset! DCM i j 42.0))))


true => truthy)




(defmacro E [z nx ny]
  `(submatrix ~z [[1 ~nx] [1 ~ny]]))

(defmacro E-2 [z nx ny]
  `(submatrix-2 ~z [[1 ~nx] [1 ~ny]]))

(defmacro E-3 [z nx ny]
  `(submatrix-3 ~z [[1 ~nx] [1 ~ny]]))


(fact "done." true => truthy)
