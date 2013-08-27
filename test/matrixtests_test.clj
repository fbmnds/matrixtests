(ns matrixtests-test
  (:require [clojure.test :refer :all]
            [matrixtests :refer :all]
            [criterium.core :as crit]
            [clatrix.core :as clx]
            [clojure.core.matrix :as M])
  (:use midje.sweet)
  (:import [org.jblas DoubleMatrix]))


(set! *warn-on-reflection* true)


;; save settings
;;
(def print-length *print-length*)
(set! *print-length* 120)
;;
;; set consistent en_US number formats for number/string conversions
;;
(def en_US (java.util.Locale. "en" "US"))
(def locale (java.util.Locale/getDefault))
(if-not (= locale en_US)
  (java.util.Locale/setDefault en_US))



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

(defmacro once-1M [expr]
  `(time (dotimes [i# 1000] (dotimes [j# 1000] '~expr))))

(defmacro bench-1K [expr]
  `(crit/bench (dotimes [i# 1000] '~expr)))

(defmacro once-1K [expr]
  `(time (dotimes [i# 1000] '~expr)))

(defmacro once [expr]
  `(time '~expr))


(def nx 1000)
(def ny 1000)
(def nx-1 (dec nx))
(def ny-1 (dec ny))
(def nx-2 (dec nx-1))
(def ny-2 (dec ny-1))
(def dd (make-array Double/TYPE nx ny))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))


(fact "aget! variants" :aget

      ;; too slow without hints
      ;;
      ;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (aget dd i j))))
      ;;(crit/bench (dotimes [i 1000] (dotimes [j 1000] (-> dd (aget i) (aget j)))))

      (println "\n- optimal hints on aget, CGrande")
      (println   "--------------------------------")
      (println "once-only reference:")
      (once-1M  (let [#^doubles a (aget #^objects dd 'i#)] (aget a 'j#)))
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aget a 'j#)))

      (println "\n- optimal hints/macros on aget, CGrande")
      (println   "---------------------------------------")
      (println "once-only reference:")
      (once-1M  (deep-aget doubles dd 'i# 'j#))
      (bench-1M (deep-aget doubles dd 'i# 'j#))

      (println "\n- optimal hints/macros/cg-aget! on aget, CGrande")
      (println   "------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (cg-aget! dd 'i# 'j#))
      (bench-1M (cg-aget! dd 'i# 'j#))

      (println "\n- optimal hints/macros/f-cg-aget! on aget, CGrande")
      (println   "--------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (f-cg-aget! dd 'i# 'j#))
      (bench-1M (f-cg-aget! dd 'i# 'j#))

      (println "\n- optimal hints/macros/cg-aget! on aget, LJensen")
      (println   "------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (lj-aget! dd 'i# 'j#))
      (bench-1M (lj-aget! dd 'i# 'j#))

      true => truthy)


(fact "aset! variants" :aset

      ;; too slow without hints
      ;;(bench-1M (aset dd i j 42.0))))

      ;; still too slow
      ;;
      ;;(println "- hints on aset, CGrande")
      ;;(bench-1M (-> #^objects dd (aget i) (aset j 42.0)))))

      (println "\n- improved hints on aset, CGrande")
      (println   "---------------------------------")
      (println "once-only reference:")
      (once-1M  (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# 42.0)))
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# 42.0)))

      (println "\n- optimal hints on aset, CGrande")
      (println   "--------------------------------")
      (println "once-only reference:")
      (once-1M  (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# (double 42.0))))
      (bench-1M (let [#^doubles a (aget #^objects dd 'i#)] (aset a 'j# (double 42.0))))

      (println "\n- optimal hints/macros on aset, CGrande")
      (println   "---------------------------------------")
      (println "once-only reference:")
      (once-1M  (deep-aset doubles dd 'i# 'j# 42.0))
      (bench-1M (deep-aset doubles dd 'i# 'j# 42.0))

      (println "\n- optimal hints/macros/cg-aset! on aset, CGrande")
      (println   "------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (cg-aset! dd 'i# 'j# 42.0))
      (bench-1M (cg-aset! dd 'i# 'j# 42.0))

      (println "\n- optimal hints/macros/f-cg-aset! on aset, CGrande")
      (println   "--------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (f-cg-aset! dd 'i# 'j# 42.0))
      (bench-1M (f-cg-aset! dd 'i# 'j# 42.0))

      (println "\n- optimal hints/macros/jl-aset! on aset, LJensen")
      (println   "------------------------------------------------")
      (println "once-only reference:")
      (once-1M  (lj-aset! dd 'i# 'j# 42.0))
      (bench-1M (lj-aset! dd 'i# 'j# 42.0))

      true => truthy)


(fact "Clatrix" :clx

      (println "\n- Clatrix get")
      (println   "-------------")
      (println "once-only reference:")
      (once-1M  (clx/get DD 'i# 'j#))
      (bench-1M (clx/get DD 'i# 'j#))

      (println "\n- Clatrix set")
      (println   "-------------")
      (println "once-only reference:")
      (once-1M  (clx/set DD 'i# 'j# 42.0))
      (bench-1M (clx/set DD 'i# 'j# 42.0))

      true => truthy)


(fact "core.matrix" :mikera

      (println "\n- core.matrix mget")
      (println   "------------------")
      (once-1M  (M/mget DCM 'i# 'j#))
      (println "once-only reference:")
      (bench-1M (M/mget DCM 'i# 'j#))

      (println "\n- core.matrix mset")
      (println   "------------------")
      (once-1M  (M/mset DCM 'i# 'j# 42.0))
      (bench-1M (M/mset DCM 'i# 'j# 42.0))

      (println "\n- core.matrix mset!")
      (println   "-------------------")
      (println "once-only reference:")
      (once-1M  (M/mset! DCM 'i# 'j# 42.0))
      (bench-1M (M/mset! DCM 'i# 'j# 42.0))

      true => truthy)


(fact "get submatrix Clatrix / Clatrix" :submatrix

      (println "\n- use Clatrix matrix")
      (println   "--------------------")
      (println "once-only reference:")
      (once-1K  (E-1 DD nx-2 ny-2))
      (bench-1K (E-1 DD nx-2 ny-2))

      (println "\n- use \"[[D\", finally cast to Clatrix matrix")
      (println   "---------------------------------------------")
      (println "once-only reference:")
      (once-1K  (E-2 DD nx-2 ny-2))
      (bench-1K (E-2 DD nx-2 ny-2))

      (println "\n- use Clatrix/from-indices")
      (println   "--------------------------")
      (println "once-only reference:")
      (once-1K  (E-3 DD nx-2 ny-2))
      (bench-1K (E-3 DD nx-2 ny-2))

      true => truthy)


(fact "Clatrix +/-/* on Clatrix matrix" :clxops

      (println "\n- use Clatrix/+")
      (println   "---------------")
      (println "once-only reference:")
      (once-1K  (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (println "\n- use Clatrix/-")
      (println   "---------------")
      (println "once-only reference:")
      (once-1K  (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (println "\n- use Clatrix/*")
      (println   "---------------")
      (println "once-only reference:")
      (once-1K  (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      true => truthy)


(fact "core.matrix add/sub/mmul on Clatrix matrix" :mtxclxops

      (println "\n- use core.matrix/add")
      (println   "---------------------")
      (println "once-only reference:")
      (once-1K  (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (println "\n- use core.matrix/sub")
      (println   "---------------------")
      (println "once-only reference:")
      (once-1K  (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (println "\n- use core.matrix/mmul")
      (println   "----------------------")
      (println "once-only reference:")
      (once-1K  (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
      (bench-1K (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      true => truthy)


(fact "core.matrix add/sub/mmul on core.matrix/matrix" :mtxops

      (println "\n- use core.matrix/add")
      (println   "---------------------")
      (println "once-only reference:")
      (once-1K  (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
      (bench-1K (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      (println "\n- use core.matrix/sub")
      (println   "---------------------")
      (println "once-only reference:")
      (once-1K  (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
      (bench-1K (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      (println "\n- use core.matrix/mmul")
      (println   "----------------------")
      (println "once-only reference:")
      (once-1K  (M/mmul (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
      (bench-1K (M/mmul (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      true => truthy)


;; restore settings
;;

(set! *print-length* print-length)
;;
(if-not (= locale en_US)
  (java.util.Locale/setDefault locale))
