(ns matrixtests
  (:require [clatrix.core :as clx]
            [clojure.core.matrix :as M]
            ;;            [clojure.core.matrix.operators]
            [clojure.core.matrix.impl default double-array ndarray persistent-vector wrappers sparse-map]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.multimethods :as mm]
            [mikera.vectorz.matrix-api]
            [mikera.vectorz.core]
            [mikera.vectorz.matrix]
            [criterium.core :as crit])
  (:use (incanter core))
  (:import [org.jblas DoubleMatrix]
           [clatrix.core Matrix]))

(M/set-current-implementation :clatrix)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
;;
(defmacro deep-aget
  ([hint array idx]
     `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
     `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
        (deep-aget ~hint a# ~@idxs))))

(defmacro deep-aset [hint array & idxsv]
  (let [hints '{doubles double ints int} ; writing a comprehensive map is left as an exercise to the reader
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                       array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))


(defn get-by-index
  "Given matrix `m`, get a single value by row index `r` and column index `c`."
  ([^Matrix m ^long r ^long c]
     (clx/dotom .get m r c)))


(defmacro m-get-by-index
  "Given matrix `m`, get a single value by row index `r` and column index `c`."
  [m r c]
  `(clx/dotom .get ~m ~r ~c))


(defn get-by-index--
  "Given matrix `m`, get a single value by row index `r` and column index `c`."
  [m r c]
  (if (fn? m)
    (let [[mm x dx y dy] (m)]
      (clx/dotom .get mm (+ x r) (+ y c)))
    (clx/dotom .get m r c)))

(defn get-by-index-
  "Given matrix `m`, get a single value by row index `r` and column index `c`."
  [m r c]
  (if (fn? m)
    (clx/dotom .get (first (m)) (+ (second (m)) r) (+ (nth (m) 3) c))
    (clx/dotom .get m r c)))


(defprotocol PGetByIndex
  (p-get-by-index [m i j]))

(extend-protocol PGetByIndex
  clojure.lang.IFn
  (p-get-by-index [m r c]
    (m-get-by-index (first (m)) (+ (second (m)) r) (+ (nth (m) 3) c)))
  Matrix
  (p-get-by-index [m r c]
    (m-get-by-index m r c)))


;; copied submatrix
;;
(defmacro submatrix
  [m [[x lx] [y ly]]]
  `(let [subm# (clx/matrix (DoubleMatrix. ~lx ~ly))]
    (dotimes [i# ~lx]
      (dotimes [j# ~ly]
        (clx/dotom .put subm# i# j# (clx/dotom .get ~m (+ i# ~x) (+ j# ~y)))))
    subm#))


;; in-place submatrix
;;
(defn submatrix! [m [[x dx] [y dy]]]
  (fn [] [m x dx y dy]))
