(ns matrixtests-test
  (:require [clojure.test :refer :all]
            [matrixtests :refer :all]
            [criterium.core :as crit]
            [clatrix.core :as clx]
            [clojure.core.matrix :as M]
            [clojure.core.matrix.impl default double-array ndarray persistent-vector wrappers sparse-map sequence]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.multimethods :as mm]
            [mikera.vectorz.matrix-api]
            [mikera.vectorz.core]
            [mikera.vectorz.matrix]
            [clojure.string :as str]
            [clojure.inspector :as insp]
            [clojure.tools.macro :as mac])
  (:use midje.sweet)
  (:import [org.jblas DoubleMatrix]))

(M/set-current-implementation :clatrix)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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


(defmacro bench-nx-ny [expr]
  `(crit/bench (dotimes [~'i nx] (dotimes [~'j ny] ~expr))))

(defmacro once-nx-ny [expr]
  `(measure (dotimes [~'i nx] (dotimes [~'j ny] ~expr))))

(defmacro bench-nx [expr]
  `(crit/bench (dotimes [~'i nx] ~expr)))

(defmacro once-nx [expr]
  `(measure (dotimes [~'i nx] ~expr)))

(defmacro bench-x-y [x y expr]
  `(crit/bench (dotimes [~'i ~x] (dotimes [~'j ~x] ~expr))))

(defmacro once-x-y [x y expr]
  `(measure (dotimes [~'i ~x] (dotimes [~'j ~x] ~expr))))

(defmacro bench-x [x expr]
  `(crit/bench (dotimes [~'i ~x] ~expr)))

(defmacro once-x [x expr]
  `(measure (dotimes [~'i ~x] ~expr)))

(defmacro once [expr]
  `(measure ~expr))

(defmacro bench [expr]
  `(crit/bench ~expr))



(def start (. System (nanoTime)))

(underline "current core.matrix implementation")
(println (M/current-implementation) "\n")

(underline "inititalizing the tests:")

(def nx 50)
(def ny 50)
(def nx-1 (dec nx))
(def ny-1 (dec ny))
(def nx-2 (dec nx-1))
(def ny-2 (dec ny-1))
(def dd (make-array Double/TYPE nx ny))
(once-nx-ny (cg-aset! dd i j 42.0))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix DD))

(def CNULL  (clx/matrix [[0.0 0.0] [0.0 0.0]]))
(def CMNULL (M/new-matrix 2 2))

(def T   [[42.0 42.0] [42.0 42.0]])
(def T*2 [[84.0 84.0] [84.0 84.0]])
(def T*T [[3528.0 3528.0] [3528.0 3528.0]])

(def CT   (clx/matrix T))
(def CT*2 (clx/matrix T*2))
(def CT*T (clx/matrix T*T))

(def CMT   (M/matrix T))
(def CMT*2 (M/matrix T*2))
(def CMT*T (M/matrix T*T))

(def A (clx/from-indices 5 5 *))

(def B   (clx/rand 10 5))
(def Bt  (clx/t B))
(def BtB (clx/* Bt B))

(println "initialisation done\n")


(fact-group
 :ftests

 (fact "" (underline "functional tests")
       => nil)

 (let [subm (submatrix A [[1 4] [1 4]])]
   (fact "submatrix"
         [(clx/get subm 0 0)
          (clx/get subm 3 0)
          (clx/get subm 0 3)
          (clx/get subm 3 3)]
         => [1.0 4.0 4.0 16.0])

   (fact "m-get-by-index"
         [(m-get-by-index subm 0 0)
          (m-get-by-index subm 3 0)
          (m-get-by-index subm 0 3)
          (m-get-by-index subm 3 3)]
         => [1.0 4.0 4.0 16.0])

   (fact "p-get-by-index" :cand
         [(p-get-by-index subm 0 0)
          (p-get-by-index subm 3 0)
          (p-get-by-index subm 0 3)
          (p-get-by-index subm 3 3)]
         => [1.0 4.0 4.0 16.0]))

 (let [fsubm (submatrix! A [[1 4] [1 4]])]
   (fact "submatrix! / p-get-by-index" :cand
         [(p-get-by-index fsubm 0 0)
          (p-get-by-index fsubm 3 0)
          (p-get-by-index fsubm 0 3)
          (p-get-by-index fsubm 3 3)]
         => [1.0 4.0 4.0 16.0]))

 (let [subm (clx/get A [1 2 3 4] [1 2 3 4])]
   (fact "submatrix by Clatrix/get" :cand
         [(clx/get subm 0 0)
          (clx/get subm 3 0)
          (clx/get subm 0 3)
          (clx/get subm 3 3)]
         => [1.0 4.0 4.0 16.0])))


(fact-group
 :get

 (fact "" (underline "get matrix element by indices")
       => nil)

 (fact "performance benchmark deep-aget, CGrande"
       (once-nx-ny  (deep-aget doubles dd i j))
       (bench-nx-ny (deep-aget doubles dd i j))
       true => truthy)

 (fact "performance core.matrix :clatrix mget"
       (once-nx-ny  (M/mget DCM i j))
       (bench-nx-ny (M/mget DCM i j))
       true => truthy)

 (fact "performance Clatrix get"
       (once-nx-ny  (clx/get DD i j))
       (bench-nx-ny (clx/get DD i j))
       true => truthy)

 (fact "performance get-by-index"
       (once-nx-ny  (get-by-index DD i j))
       (bench-nx-ny (get-by-index DD i j))
       true => truthy)

 (fact "performance p-get-by-index"
       (once-nx-ny  (p-get-by-index DD i j))
       (bench-nx-ny (p-get-by-index DD i j))
       true => truthy)

 (fact "performance m-get-by-index"
       (once-nx-ny  (m-get-by-index DD i j))
       (bench-nx-ny (m-get-by-index DD i j))
       true => truthy)

 (fact
  (class DD) => (class DCM)))


(fact-group
 :create

 (fact "" (underline "create a matrix")
       => nil)

 (fact "performance benchmark JBLAS DoubleMatrix[i,j]"
       (once  (DoubleMatrix. 500 500))
       (bench (DoubleMatrix. 500 500))
       true => truthy)

 (fact "Clatrix create matrix"
       (once  (clx/matrix (DoubleMatrix. 500 500)))
       (bench (clx/matrix (DoubleMatrix. 500 500)))
       true => truthy))

(fact-group
 :submatrix

 (fact "" (underline "create and access a submatrix")
       => nil)

 (defmacro subm-test []
   `(let [subm# (clx/matrix DD (range nx-2) (range ny-2))]
                (dotimes [i# nx-2]
                  (dotimes [j# ny-2]
                    (clx/get subm# i# j#)))))
 (fact "performance benchmark Clatrix/get"
       (once  (subm-test))
       (bench (subm-test))
       true => truthy)

 (defmacro subm-cand-test []
   `(let [subm# (submatrix! DD [[0 nx-2] [0 ny-2]])]
                (dotimes [i# nx-2]
                  (dotimes [j# ny-2]
                    (p-get-by-index subm# i# j#)))))
 (fact "performance of submatrix! / p-get-by-index"
       (once  (subm-cand-test))
       (bench (subm-cand-test))
       true => truthy)

  (defmacro submatrix-cand-test []
   `(let [subm# (submatrix DD [[0 nx-2] [0 ny-2]])]
                (dotimes [i# nx-2]
                  (dotimes [j# ny-2]
                    (m-get-by-index subm# i# j#)))))
 (fact "performance of submatrix / m-get-by-index"
       (once  (submatrix-cand-test))
       (bench (submatrix-cand-test))
       true => truthy)

 )



(println)
(underline "Total elapsed time for all tests")
(println " : " (/ (double (- (. System (nanoTime)) start)) 1000000.0) " ms.")


;; restore settings
;;

(set! *print-length* print-length)
;;
(if-not (= locale en_US)
  (java.util.Locale/setDefault locale))
