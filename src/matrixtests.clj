(ns matrixtests
  (:require [clatrix.core :as clx]
            [clojure.core.matrix :as M]
            [criterium.core :as crit])
  (:use (incanter core))
  (:import [org.jblas DoubleMatrix]))


;; http://clj-me.cgrand.net/2009/10/15/multidim-arrays/
;;
;; (defn array? [x] (-> x class .isArray))
;; (defn see [x] (if (array? x) (map see x) x))
;;
;; (see (into-array (map (partial into-array Double/TYPE) [[1 2 3 4] [5 6]])))
;; ;; ((1.0 2.0 3.0 4.0) (5.0 6.0))
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


(defmacro cg-aget! [M i j]
  `(deep-aget ~'doubles ~M ~i ~j))

(defmacro cg-aset! [M i j v]
  `(deep-aset ~'doubles ~M ~i ~j ~v))

(defn f-cg-aget! [M i j]
  (deep-aget doubles M i j))

(defn f-cg-aset! [M i j v]
  (deep-aset doubles M i j v))

;; http://www.bestinclass.dk/index.clj/2010/03/functional-fluid-dynamics-in-clojure.html
;;
(defmacro lj-aget!
  ([array y]      `(aget ~(vary-meta array assoc :tag 'doubles) ~y))
  ([array x & ys] `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~@ys)]
                     (lj-aget! a# ~x))))
;;
(defmacro lj-aset! [array x y v]
  (let [nested-array `(aget ~(vary-meta array assoc :tag 'objects) ~y)
        a-sym         (with-meta (gensym "a") {:tag 'doubles})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~x (double ~v)))))


(defmacro submatrix-1 [M [[a nx] [b ny]]]
  `(let [m# (clx/matrix (DoubleMatrix. ~nx ~ny
                                       (make-array Double/TYPE (* ~nx ~ny))))]
     (dotimes [i# ~nx]
       (dotimes [j# ~ny]
         (clx/set m# i# j# (clx/get ~M (+ ~a i#) (+ ~b j#)))))
     m#))

(defmacro E-1 [z nx ny]
  `(submatrix-1 ~z [[1 ~nx] [1 ~ny]]))


(defmacro submatrix-2 [M [[a nx] [b ny]]]
  `(let [m# (make-array Double/TYPE ~nx ~ny)]
     (dotimes [i# ~nx]
       (dotimes [j# ~ny]
         (cg-aset! m# i# j# (clx/get ~M (+ ~a i#) (+ ~b j#)))))
     (clx/matrix (DoubleMatrix. ^"[[D" m#))))

(defmacro E-2 [z nx ny]
  `(submatrix-2 ~z [[1 ~nx] [1 ~ny]]))


(defmacro submatrix-3 [M [[a nx] [b ny]]]
  `(clx/from-indices ~nx ~ny (fn [x# y#] (clx/get ~M (+ ~a x#) (+ ~b y#)))))

(defmacro E-3 [z nx ny]
  `(submatrix-3 ~z [[1 ~nx] [1 ~ny]]))
