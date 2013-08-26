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

(defmacro bench-1M [expr]
  `(crit/bench (dotimes [i# 1000] (dotimes [j# 1000] '~expr))))

(defmacro bench-1K [expr]
  `(crit/bench (dotimes [i# 1000] '~expr)))

(def dd (make-array Double/TYPE 1000 1000))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))


(fact "aget! variants" :aget

      ;; too slow without hints
      ;;
      ;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (aget dd i j))))
      ;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (-> dd (aget i) (aget j)))))

      (println "- optimal hints on aget, CGrande")
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aget a 'j#)))

      (println "- optimal hints/macros on aget, CGrande")
      (bench-1M (deep-aget doubles dd 'i# 'j#))

      (println "- optimal hints/macros/cg-aget! on aget, CGrande")
      (bench-1M (cg-aget! dd 'i# 'j#))

      (println "- optimal hints/macros/f-cg-aget! on aget, CGrande")
      (bench-1M (f-cg-aget! dd 'i# 'j#))

      (println "- optimal hints/macros/cg-aget! on aget, LJensen")
      (bench-1M (lj-aget! dd 'i# 'j#))

      true => truthy)


(fact "aset! variants" :aset

      ;; too slow without hints
      ;;(bench-1M (aset dd i j 42.0))))

      ;; still too slow
      ;;
      ;;(println "- hints on aset, CGrande")
      ;;(bench-1M (-> #^objects dd (aget i) (aset j 42.0)))))

      (println "- improved hints on aset, CGrande")
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# 42.0)))

      (println "- optimal hints on aset, CGrande")
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# (double 42.0))))

      (println "- optimal hints/macros on aset, CGrande")
      (bench-1M (deep-aset doubles dd 'i# 'j# 42.0))

      (println "- optimal hints/macros/cg-aset! on aset, CGrande")
      (bench-1M (cg-aset! dd 'i# 'j# 42.0))

      (println "- optimal hints/macros/f-cg-aset! on aset, CGrande")
      (bench-1M (f-cg-aset! dd 'i# 'j# 42.0))

      (println "- optimal hints/macros/jl-aset! on aset, LJensen")
      (bench-1M (lj-aset! dd 'i# 'j# 42.0))

      true => truthy)


(fact "Clatrix" :clx

      (println "- Clatrix get")
      (bench-1M (clx/get DD 'i# 'j#))

      (println "- Clatrix set")
      (bench-1M (clx/set DD 'i# 'j# 42.0))

      true => truthy)


(fact "core.matrix" :mikera

      (println "- core.matrix mget")
      (bench-1M (M/mget DCM 'i# 'j#))

      (println "- core.matrix mset")
      (bench-1M (M/mset DCM 'i# 'j# 42.0))

      (println "- core.matrix mset!")
      (bench-1M (M/mset! DCM 'i# 'j# 42.0))

      true => truthy)


(fact "get submatrix Clatrix / Clatrix" :submatrix
      (println "- use Clatrix matrix")
      (bench-1K (E-1 DD 1000 1000))

      (println "- use \"[[D\", finally cast to Clatrix matrix")
      (bench-1K (E-2 DD 1000 1000))

      (println "-use Clatrix/from-indices")
      (bench-1K (E-3 DD 1000 1000))

      true => truthy)
