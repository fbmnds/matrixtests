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


(defmacro header [x]
  `(let [x# (str ~x)
         l# (+ 2 (count x#))]
     (println (str/join ["\n- " x#]))
     (println (str/join (vec (repeat l# "-"))))
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

(defmacro bench-1M [expr]
  `(crit/bench (dotimes [i# 1000] (dotimes [j# 1000] '~expr))))

(defmacro once-1M [expr]
  `(measure (dotimes [i# 1000] (dotimes [j# 1000] ~expr))))

(defmacro bench-1K [expr]
  `(crit/bench (dotimes [i# 1000] '~expr)))

(defmacro once-1K [expr]
  `(measure (dotimes [i# 1000] '~expr)))

(defmacro once [expr]
  `(time '~expr))


(def nx 100)
(def ny 100)
(def nx-1 (dec nx))
(def ny-1 (dec ny))
(def nx-2 (dec nx-1))
(def ny-2 (dec ny-1))
(def dd (make-array Double/TYPE nx ny))
(def DD (clx/matrix (DoubleMatrix. ^"[[D" dd)))
(def DCM (M/matrix dd))

;; (defmacro with-open
;;   "bindings => [name init ...]
;;
;; Evaluates body in a try expression with names bound to the values
;; of the inits, and a finally clause that calls (.close name) on each
;; name in reverse order."
;;   {:added "1.0"}
;;   [bindings & body]
;;   (assert-args
;;      (vector? bindings) "a vector for its binding"
;;      (even? (count bindings)) "an even number of forms in binding vector")
;;   (cond
;;     (= (count bindings) 0) `(do ~@body)
;;     (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
;;                               (try
;;                                 (with-open ~(subvec bindings 2) ~@body)
;;                                 (finally
;;                                   (. ~(bindings 0) close))))
;;     :else (throw (IllegalArgumentException.
;;                    "with-open only allows Symbols in bindings"))))
;;
;;
;; (fact "test" :test
;;       (measure
;;        (dotimes [i 100]
;;          (dotimes [j 100]
;;            (let [#^doubles a (aget #^objects dd i)] (aget a j)))))
;;       => nil)

(fact "aget! variants" :aget

      ;; too slow without hints
      ;;
      ;;(crit/bench (dotimes [i 100] (dotimes [j 100] (aget dd i j))))
      ;;(crit/bench (dotimes [i 100] (dotimes [j 100] (-> dd (aget i) (aget j)))))

      (header "optimal hints on aget, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aget a j)))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aget a j)))))
;;       (once-1M  `(let [#^doubles a (aget #^objects dd i#)] (aget a j#)))
;;       (bench-1M `(let [#^doubles a (aget #^objects dd i#)] (aget a j#)))

      (header "optimal hints/macros on aget, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (deep-aget doubles dd i j))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (deep-aget doubles dd i j))))
;;       (once-1M  (deep-aget doubles dd i j))
;;       (bench-1M (deep-aget doubles dd i j))

      (header "optimal hints/macros/cg-aget! on aget, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (cg-aget! dd i j))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (cg-aget! dd i j))))
;;       (once-1M  (cg-aget! dd i j))
;;       (bench-1M (cg-aget! dd i j))

      (header "optimal hints/macros/f-cg-aget! on aget, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (f-cg-aget! dd i j))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (f-cg-aget! dd i j))))
;;       (once-1M  (f-cg-aget! dd i j))
;;       (bench-1M (f-cg-aget! dd i j))

      (header "optimal hints/macros/cg-aget! on aget, LJensen")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (lj-aget! dd i j))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (lj-aget! dd i j))))
;;       (once-1M  (lj-aget! dd i j))
;;       (bench-1M (lj-aget! dd i j))

      true => truthy)


(fact "aset! variants" :aset

      ;; too slow without hints
      ;;(bench-1M (aset dd i j 42.0))))

      ;; still too slow
      ;;
      ;;(println "- hints on aset, CGrande")
      ;;(bench-1M (-> #^objects dd (aget i) (aset j 42.0)))))

      (header "improved hints on aset, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))))
;;       (once-1M  (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))
;;       (bench-1M (let [#^doubles a (aget #^objects dd i)] (aset a j 42.0)))

      (header "optimal hints on aset, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))))
;;       (once-1M  (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))
;;       (bench-1M (let [#^doubles a (aget #^objects dd i)] (aset a j (double 42.0))))

      (header "optimal hints/macros on aset, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (deep-aset doubles dd i j 42.0))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (deep-aset doubles dd i j 42.0))))
;;       (once-1M  (deep-aset doubles dd i j 42.0))
;;       (bench-1M (deep-aset doubles dd i j 42.0))

      (header "optimal hints/macros/cg-aset! on aset, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (cg-aset! dd i j 42.0))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (cg-aset! dd i j 42.0))))
;;       (once-1M  (cg-aset! dd i j 42.0))
;;       (bench-1M (cg-aset! dd i j 42.0))

      (header "optimal hints/macros/f-cg-aset! on aset, CGrande")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (f-cg-aset! dd i j 42.0))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (f-cg-aset! dd i j 42.0))))
;;       (once-1M  (f-cg-aset! dd i j 42.0))
;;       (bench-1M (f-cg-aset! dd i j 42.0))

      (header "optimal hints/macros/jl-aset! on aset, LJensen")
      (measure
       (dotimes [i 100]
         (dotimes [j 100]
           (lj-aset! dd i j 42.0))))
      (crit/bench
       (dotimes [i 100]
         (dotimes [j 100]
           (lj-aset! dd i j 42.0))))
;;       (once-1M  (lj-aset! dd i j 42.0))
;;       (bench-1M (lj-aset! dd i j 42.0))

      true => truthy)


(fact "Clatrix" :clx

      (header "Clatrix get")
      (measure
       (dotimes [i 10]
         (dotimes [j 10]
           (clx/get DD i j))))
      (crit/bench
       (dotimes [i 10]
         (dotimes [j 10]
           (clx/get DD i j))))
;;       (once-1M  (clx/get DD i j))
;;       (bench-1M (clx/get DD i j))

      (header "Clatrix set")
      (measure
       (dotimes [i 10]
         (dotimes [j 10]
           (clx/set DD i j 42.0))))
      (crit/bench
       (dotimes [i 10]
         (dotimes [j 10]
           (clx/set DD i j 42.0))))
;;       (once-1M  (clx/set DD i j 42.0))
;;       (bench-1M (clx/set DD i j 42.0))

      true => truthy)


(fact "core.matrix" :mikera

      (header "core.matrix mget")
      (measure
       (dotimes [i 10]
         (dotimes [j 10]
           (M/mget DCM i j))))
      (crit/bench
       (dotimes [i 10]
         (dotimes [j 10]
           (M/mget DCM i j))))
      ;;       (once-1M  (M/mget DCM i j))
      ;;       (bench-1M (M/mget DCM i j))

      (header "core.matrix mset")
      (measure
       (dotimes [i 2]
         (dotimes [j 2]
           (M/mset DCM i j 42.0))))
      (crit/bench
       (dotimes [i 2]
         (dotimes [j 2]
           (M/mset DCM i j 42.0))))
      ;;       (once-1M  (M/mset DCM i j 42.0))
      ;;       (bench-1M (M/mset DCM i j 42.0))


;; core.matrix/mset! is not yet implemented for Clatrix:
;;
;; IllegalArgumentException No implementation of method: :set-2d! of protocol:
;; #'clojure.core.matrix.protocols/PIndexedSettingMutable found for class: clojure.lang.LazySeq
;; clojure.core/-cache-protocol-fn (core_deftype.clj:541)
;;
      (try
        (header "core.matrix mset!")
        (measure
         (dotimes [i 10]
           (dotimes [j 10]
             (M/mset! DCM i j 42.0))))
        (catch Exception e (str "caught exception: " (.getMessage e))))
      (try
        (crit/bench
         (dotimes [i 10]
           (dotimes [j 10]
             (M/mset! DCM i j 42.0))))
        (catch Exception e (str "caught exception: " (.getMessage e))))
      ;;       (once-1M  (M/mset! DCM i j 42.0))
      ;;       (bench-1M (M/mset! DCM i j 42.0))

      true => truthy)


(fact "get submatrix Clatrix / Clatrix" :submatrix

      (header "use Clatrix matrix")
      (measure
       (dotimes [i 2]
         (E-1 DD nx-2 ny-2)))
      (crit/bench
       (dotimes [i 2]
         (E-1 DD nx-2 ny-2)))
;;       (once-1K  (E-1 DD nx-2 ny-2))
;;       (bench-1K (E-1 DD nx-2 ny-2))

      (header "use \"[[D\", finally cast to Clatrix matrix")
      (measure
       (dotimes [i 2]
         (E-2 DD nx-2 ny-2)))
      (crit/bench
       (dotimes [i 2]
         (E-2 DD nx-2 ny-2)))
;;       (once-1K  (E-2 DD nx-2 ny-2))
;;       (bench-1K (E-2 DD nx-2 ny-2))

      (header "use Clatrix/from-indices")
      (measure
       (dotimes [i 2]
         (E-3 DD nx-2 ny-2)))
      (crit/bench
       (dotimes [i 2]
         (E-3 DD nx-2 ny-2)))
;;       (once-1K  (E-3 DD nx-2 ny-2))
;;       (bench-1K (E-3 DD nx-2 ny-2))

      true => truthy)


(fact "Clatrix +/-/* on Clatrix matrix" :clxops

      (header "use Clatrix/+")
      (measure
       (dotimes [i 10]
         (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (clx/+ (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (header "use Clatrix/-")
      (measure
       (dotimes [i 10]
         (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (clx/- (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (header "use Clatrix/*")
      (measure
       (dotimes [i 10]
         (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (clx/* (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      true => truthy)


(fact "core.matrix add/sub/mmul on Clatrix matrix" :mtxclxops

      (header "use core.matrix/add")
      (measure
       (dotimes [i 10]
         (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (M/add (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      (header "use core.matrix/sub")
      (measure
       (dotimes [i 10]
         (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (M/sub (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))


;; CompilerException java.lang.RuntimeException:
;; No such var: M/mmul, compiling:(matrixtests_test.clj:394:1)
;;
;;       (header "use core.matrix/mmul")
;;       (measure
;;        (dotimes [i 10]
;;          (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (catch Exception e (str "caught exception: " (.getMessage e)))
;;       (crit/bench
;;        (dotimes [i 10]
;;          (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2))))
;;       (once-1K  (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))
;;       (bench-1K (M/mmul (E-1 DD nx-2 ny-2) (E-1 DD nx-2 ny-2)))

      true => truthy)


(fact "core.matrix add/sub/mmul on core.matrix/matrix" :mtxops

      (header "use core.matrix/add")
      (measure
       (dotimes [i 10]
         (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
;;       (once-1K  (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
;;       (bench-1K (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      (header "use core.matrix/sub")
      (measure
       (dotimes [i 10]
         (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
;;       (once-1K  (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
;;       (bench-1K (M/sub (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      (header "use core.matrix/mmul")
      (measure
       (dotimes [i 10]
         (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
      (crit/bench
       (dotimes [i 10]
         (M/add (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2))))
;;       (once-1K  (M/mmul (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))
;;       (bench-1K (M/mmul (E-1 DCM nx-2 ny-2) (E-1 DCM nx-2 ny-2)))

      true => truthy)


;; restore settings
;;

(set! *print-length* print-length)
;;
(if-not (= locale en_US)
  (java.util.Locale/setDefault locale))
