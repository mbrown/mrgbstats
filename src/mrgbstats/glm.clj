(ns mrgbstats.glm
  ;(:require [clojure.core.matrix :as matrix])
  )

; General linear model (GLM) functions

(defn dot-product
  "Returns dot product.
  a, b = vectors"
  [a b]
  (reduce + (mapv * a b)))

(defn make-linear-slope-computer
  "Returns function taking input vector y and returning slope parameter for
  linear model y = slope * x + intercept. Mean-centres x.
  x = vector"
  [x]
  (if (zero? (count x))
    (do
      (println "WARNING: in mrgbstats.glm/make-linear-slope-computer, x is empty. Slope will be NaN.")
      (fn [y]
        Double/NaN))
    (let
      ; First mean-centre x.
      ; Only need the first row of the hat matrix (scaled-x-t below).
      ; Don't care about the intercept, which is just the mean of y.
      [n (count x)
       mean-x (/ (double (reduce + x)) n)
       centred-x (mapv #(- (double %) mean-x) x)
       dot-prod (dot-product centred-x centred-x)
       ;scaled-x-t (mapv #(/ % dot-prod) centred-x)
       ]
      (if (zero? dot-prod)
        (do
          (println "WARNING: in mrgstats.glm/make-linear-slope-computer, dot product is zero. Slope will be NaN.")
          (fn [y]
            Double/NaN))
        (fn [y]
          ;(reduce dot-prod-reducer scaled-x-t y)
          (/ (dot-product centred-x y) dot-prod))))))

