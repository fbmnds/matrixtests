(ns matrixtests-test
  (:require [clojure.test :refer :all]
            [matrixtests :refer :all]
            [criterium.core :as crit]
            [clatrix.core :as clx]
            [clojure.core.matrix :as M]
            [clojure.string :as str])
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


(defmacro underline [x]
  `(let [x# (str ~x)
         l# (count x#)]
     (println x#)
     (println (str/join (vec (repeat l# "-"))))))

(defmacro header [x]
  `(do
     (println)
     (underline (str/join ["- " (str ~x)]))
     (println "once-only reference:")))

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
     (time ~expr)))

(defmacro p-measure [expr]
  `(do
     (println (str '~expr))
     (println "msecs : " (msecs ~expr))))


(def nx 100)
(def ny 100)
(def nx-1 (dec nx))
(def ny-1 (dec ny))
(def nx-2 (dec nx-1))
(def ny-2 (dec ny-1))
(def dd (make-array Double/TYPE nx ny))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))


(defmacro bench-nx-ny [expr]
  `(crit/bench (dotimes [~'i nx] (dotimes [~'j ny] ~expr))))

(defmacro once-nx-ny [expr]
  `(measure (dotimes [~'i nx] (dotimes [~'j ny] ~expr))))

(defmacro bench-nx [expr]
  `(crit/bench (dotimes [~'i nx] ~expr)))

(defmacro once-nx [expr]
  `(measure (dotimes [~'i nx] ~expr)))

(defmacro bench-x [x expr]
  `(crit/bench (dotimes [~'i ~x] ~expr)))

(defmacro once-x [x expr]
  `(measure (dotimes [~'i ~x] ~expr)))

(defmacro once [expr]
  `(measure ~expr))

(defmacro bench [expr]
  `(crit/bench ~expr))


(def start (. System (nanoTime)))

(fact "test" :test
      (once-x 2 (println (let [#^doubles a (aget #^objects dd i)] (aget a 0))))
      => nil)


(fact "aget! variants" :aget

      ;; too slow without hints
      ;;
      ;;(crit/bench (dotimes [i 100] (dotimes [j 100] (aget dd i j))))
      ;;(crit/bench (dotimes [i 100] (dotimes [j 100] (-> dd (aget i) (aget j)))))

      (header "optimal hints on aget, CGrande")
      (once-nx-ny  (let [#^doubles a (aget #^objects dd i)] (aget a j)))
      (bench-nx-ny (let [#^doubles a (aget #^objects dd i)] (aget a j)))

      (header "optimal hints/macros on aget, CGrande")
      (once-nx-ny  (deep-aget doubles dd i j))
      (bench-nx-ny (deep-aget doubles dd i j))

      (header "optimal hints/macros/cg-aget! on aget, CGrande")
      (once-nx-ny  (cg-aget! dd i j))
      (bench-nx-ny (cg-aget! dd i j))

      (header "optimal hints/macros/f-cg-aget! on aget, CGrande")
      (once-nx-ny  (f-cg-aget! dd i j))
      (bench-nx-ny (f-cg-aget! dd i j))

      (header "optimal hints/macros/cg-aget! on aget, LJensen")
      (once-nx-ny  (lj-aget! dd i j))
      (bench-nx-ny (lj-aget! dd i j))

      true => truthy)


(fact "aset! variants" :aset

      ;; too slow without hints
      ;;(bench-1M (aset dd i j 42.0))))

      ;; still too slow
      ;;
      ;;(println "- hints on aset, CGrande")
      ;;(bench-1M (-> #^objects dd (aget i) (aset j 42.0)))))

      (header "improved hints on aset, CGrande")
      (once-nx-ny  (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))
      (bench-nx-ny (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))

      (header "optimal hints on aset, CGrande")
      (once-nx-ny  (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))
      (bench-nx-ny (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))

      (header "optimal hints/macros on aset, CGrande")
      (once-nx-ny  (deep-aset doubles dd i j 42.0))
      (bench-nx-ny (deep-aset doubles dd i j 42.0))

      (header "optimal hints/macros/cg-aset! on aset, CGrande")
      (once-nx-ny  (cg-aset! dd i j 42.0))
      (bench-nx-ny (cg-aset! dd i j 42.0))

      (header "optimal hints/macros/f-cg-aset! on aset, CGrande")
      (once-nx-ny  (f-cg-aset! dd i j 42.0))
      (bench-nx-ny (f-cg-aset! dd i j 42.0))

      (header "optimal hints/macros/jl-aset! on aset, LJensen")
      (once-nx-ny  (lj-aset! dd i j 42.0))
      (bench-nx-ny (lj-aset! dd i j 42.0))

      true => truthy)


(fact "Clatrix" :clx

      (header "Clatrix get")
      (once-nx-ny  (clx/get DD i j))
      (bench-nx-ny (clx/get DD i j))

      (header "Clatrix set")
      (once-nx-ny  (clx/set DD i j 42.0))
      (bench-nx-ny (clx/set DD i j 42.0))

      true => truthy)


(fact "core.matrix" :mikera

      (header "core.matrix mget")
      (once-nx-ny  (M/mget DCM i j))
      (bench-nx-ny (M/mget DCM i j))

      (header "core.matrix mset")
      (once-nx-ny  (M/mset DCM i j 42.0))
      ;; (bench-nx-ny (M/mset DCM i j 42.0))

      (header "core.matrix mset! is not implemented for Clatrix:")
      (try
        (once-nx-ny  (M/mset! DCM i j 42.0))
        (catch Exception e (println "caught exception: " (.getMessage e))))

        true => truthy)


(fact "get submatrix Clatrix / Clatrix" :submatrix

      (header "use Clatrix matrix")
      (once-x  2 (E-1 DD nx-2 ny-2))
      (bench-x 2 (E-1 DD nx-2 ny-2))

      (header "use \"[[D\", finally cast to Clatrix matrix")
      (once-x  2 (E-2 DD nx-2 ny-2))
      (bench-x 2 (E-2 DD nx-2 ny-2))

      (header "use Clatrix/from-indices")
      (once-x  100 (E-3 DD nx-2 ny-2))
      (bench-x 100 (E-3 DD nx-2 ny-2))

      true => truthy)


(fact "Clatrix +/-/* on Clatrix matrix" :clxops

      (header "use Clatrix/+")
      (once-nx-ny  (clx/set DD i j 42.0))
      (once  (clx/+ (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))
      (bench (clx/+ (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      (header "use Clatrix/-")
      (once  (clx/- (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))
      (bench (clx/- (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      (header "use Clatrix/*")
      (once  (clx/* (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      (bench (clx/* (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      true => truthy)


(fact "core.matrix add/sub/mmul on Clatrix matrix" :mtxclxops

      (header "use core.matrix/add")
      (once  (M/add (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))
      (bench (M/add (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      (header "use core.matrix/sub")
      (once (M/sub (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))
      (bench (M/sub (E-3 DD nx-2 ny-2) (E-3 DD nx-2 ny-2)))

      (header "use core.matrix/mmul throws compiler Exception")
      (println "CompilerException java.lang.RuntimeException:")
      (println "No such var: M/mmul, compiling:(matrixtests_test.clj:...:1)")
      (println "ref. https://github.com/mikera/matrix-api/blob/master/src/main/clojure/clojure/core/matrix.clj#L630")

      true => truthy)


(fact "core.matrix add/sub/mmul on core.matrix/matrix" :mtxops

      (header "use core.matrix/add")
      (once  (M/add (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))
      (bench (M/add (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))

      (header "use core.matrix/sub")
      (once  (M/sub (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))
      (bench (M/sub (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))

      (header "use core.matrix/mmul")
;;       (once  (M/mmul (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))
;;       (bench (M/mmul (E-3 DCM nx-2 ny-2) (E-3 DCM nx-2 ny-2)))
      (println "CompilerException java.lang.RuntimeException:")
      (println "No such var: M/mmul, compiling:(matrixtests_test.clj:...:1)")
      (println "ref. https://github.com/mikera/matrix-api/blob/master/src/main/clojure/clojure/core/matrix.clj#L630")

      true => truthy)

(println)
(underline "Total elapsed time for all tests")
(println " : " (/ (double (- (. System (nanoTime)) start)) 1000000.0) " ms.")


;; restore settings
;;

(set! *print-length* print-length)
;;
(if-not (= locale en_US)
  (java.util.Locale/setDefault locale))
