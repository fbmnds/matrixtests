(ns matrixtests-test
  (:require [clojure.test :refer :all]
            [matrixtests :refer :all]
            [criterium.core :as crit]
            [clatrix.core :as clx]
            [clojure.core.matrix :as M]
            [clojure.string :as str]
            [clojure.inspector :as insp])
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

(underline "inititalizing the tests:")

(def nx 100)
(def ny 100)
(def nx-1 (dec nx))
(def ny-1 (dec ny))
(def nx-2 (dec nx-1))
(def ny-2 (dec ny-1))
(def dd (make-array Double/TYPE nx ny))
(once-nx-ny (cg-aset! dd i j 42.0))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))

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

(fact-group
 :ftests
 (underline "functional tests")

 (fact "optimal hints on aget, CGrande"
       [(let [#^doubles a (aget #^objects dd 0)] (aget a 0))
        (let [#^doubles a (aget #^objects dd (dec nx))] (aget a (dec ny)))]
       => [42.0 42.0])

 (fact "optimal hints/macros on aget, CGrande"
       [(deep-aget doubles dd 0 0)
        (deep-aget doubles dd (dec nx) (dec ny))]
       => [42.0 42.0])

 (println "... omitting further get/set tests\n")


 (fact "clx/+ is ok for Clatrix matrix"
       (clx/+ CT CT) => CT*2)

 (try
   (do
     (println)
     (underline "clx/+ is not suitable for clojure array/matrix")
     (clx/+ T T))
   (catch Exception e (println (.getMessage e))))

 (try
   (do
     (println)
     (underline "clx/+ is not suitable for core.matrix matrix")
     (clx/+ CMT CMT))
   (catch Exception e (println (.getMessage e))))
 (println)

 (underline "core.matrix provides convenient handling of different matrix types")
 (fact "M/add is ok for core.matrix matrix"
       (M/add CMT CMT) => CMT*2)

 (fact "M/add is ok for clojure array/matrix"
       (M/add T T) => CMT*2)

 (fact "M/add is ok for Clatrix matrix"
       (M/add CT CT) => CMT*2)

 (println)
 (underline "testing matrix subtraction")
 (fact "clx/- is ok for Clatrix matrix"
       (clx/- CT CT) => CNULL)

 (fact "M/sub is ok for core.matrix matrix"
       (M/sub CMT CMT) => CMNULL)

 (fact "M/sub is ok for clojure array/matrix"
       (M/sub T T) => CMNULL)

 (fact "M/sub is ok for Clatrix matrix"
       (M/sub CT CT) => CMNULL)

 (println)
 (underline "testing matrix multiplication (inner product)")
 (fact "clx/* is ok for Clatrix matrix"
       (clx/* CT CT) => CT*T)

 (println)
 (underline "M/mul is documented to be elementwise product, but provides inner product")
 (println "ref. https://github.com/mikera/matrix-api/blob/master/src/main/clojure/clojure/core/matrix.clj#L609")
 (fact "M/mul is 'ok' for core.matrix matrix"
       (M/mul CMT CMT) => CMT*T)

 (fact "M/mul is 'ok' for clojure array/matrix"
       (M/mul T T) => CMT*T)

 (fact "M/mul is 'ok' for Clatrix matrix"
       (M/mul CT CT) => CMT*T)

 (println)
 (underline "core.matrix/mmul should be inner product, but throws compiler exception")
 (println "ref. https://github.com/mikera/matrix-api/blob/master/src/main/clojure/clojure/core/matrix.clj#L630")
 (println "CompilerException java.lang.RuntimeException:")
 (println "No such var: M/mmul, compiling:(matrixtests_test.clj:...:1)")

 (println)
 (underline "testing matrix multiplication (scalar product)")
 (fact "clx/* is ok for Clatrix matrix"
       (clx/* 2. CT) => CT*2)

 (fact "M/mul is ok for core.matrix matrix"
       (M/mul 2. CMT) => CMT*2)

 (fact "M/mul is ok for clojure array/matrix"
       (M/mul 2. T) => CMT*2)

 (fact "M/mul is ok for Clatrix matrix"
       (M/mul 2. CT) => CMT*2)

 (println)
 (underline "testing submatrices")

 )

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

      (header "optimal hints/macros/lj-aget! on aget, LJensen")
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

      (header "optimal hints/macros/lj-aset! on aset, LJensen")
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
