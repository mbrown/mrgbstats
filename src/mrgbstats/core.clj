(ns mrgbstats.core
  (:require [mrgbstats.permtest :as permtest]
            [mrgbstats.atoms :as atoms]
            [mrgbstats.glm :as glm]
            [random-seed.core :as random-seed]))

; ----------
; Basic statistics functions
(defn mean
  "a = seq or vec of numbers"
  [a]
  (/ (double (reduce + a)) (count a)))

(defn variance
  "a = seq or vec of numbers"
  [a]
  (let [m (mean a)]
    (/ (double (reduce (fn [acc x] (+ acc (* (- x m) (- x m)))) 0.0 a))
       (dec (count a)))))

(defn std
  "a = seq or vec of numbers"
  [a]
  (java.lang.Math/sqrt (variance a)))

(defn pooled-variance
  "a, b = seq or vec of numbers"
  [a b]
  (/ (+ (* (variance a) (dec (count a)))
        (* (variance b) (dec (count b))))
     (+ (count a) (count b) -2)))

(defn pooled-std
  "a, b = seq or vec of numbers"
  [a b]
  (java.lang.Math/sqrt (pooled-variance a b)))

(defn stderr
  "a = seq or vec of numbers"
  [a]
  (/ (std a)
     (java.lang.Math/sqrt (count a))))

(defn median
  "a = seq or vec of numbers"
  [a]
  (nth (sort a) (java.lang.Math/round (/ (count a) 2.0))))


; ----------
; Permutation testing API
(defn permtest-singlevar
  "Permutation testing. Tests for difference of single variable functions of
  two groups. Single variable functions could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  a and b = seqs or vecs of numbers to compare
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively.
  numiter = number of iterations
  options = hash-map, possible keys :parallel?
    parallel? = Boolean (default false). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns two-tailed p-value."
  ([a b func-a func-b numiter]
   (permtest-singlevar a b func-a func-b numiter nil))
  ([a b func-a func-b numiter options]
   (permtest/permtest-singlevar a b func-a func-b numiter options)))

(defn permtest-mean
  "Permutation testing. Tests for difference of means two groups.
  a and b = seqs or vecs of numbers to compare
  numiter = number of iterations
  options = hash-map, possible keys :parallel?
    parallel? = Boolean (default false). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns two-tailed p-value."
  ([a b numiter]
   (permtest-mean a b numiter nil))
  ([a b numiter options]
   (permtest/permtest-singlevar a b mean mean numiter options)))

(defn permtest-multiprops
  "Permutation testing for multiple categorical variables based on chi
  squared. Tests for difference of distributions between two groups.
  a and b = seqs or vecs.
  numiter = number
  options = hash-map, possible keys :parallel?
    parallel? = Boolean (default false). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns p-value."
  ([a b numiter]
   (permtest-multiprops a b numiter nil))
  ([a b numiter options]
   (permtest/permtest-multiprops a b numiter options)))

(defn permtest-linear-slope
  "Permutation testing. Compares linear slope from a single group to zero.
  linear model: y = slope * x + intercept. Mean-centres x.
  x = vector of independent variables
  y = seq or vec of dependent variables
  numiter = number of iterations
  options = hash-map, possible keys :parallel?
    parallel? = Boolean (default false). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns two-tailed p-value."
  ([x y numiter]
   (permtest-linear-slope x y numiter nil))
  ([x y numiter options]
   (permtest/permtest-oneglmparam
     y (glm/make-linear-slope-computer x) numiter options)))

; ----------
; FDR multiple comparison correction API
(defn fdr
  "pvec = vector of p-values
  alpha = corrected p-value (default 0.05)
  Performs false discovery rate (FDR) correction for multiple comparisons
  using the Benjamini-Hochberg method.
  Returns p threshold or :nothing-passes-fdr if nothing passes FDR
  correction."
  ([pvec]
   (fdr pvec 0.05))
  ([pvec alpha]
   (random-seed/set-random-seed!
     (+ (.getId (Thread/currentThread))
        (Math/abs (.hashCode (java.time.Instant/now)))))
   (let [sorted-hi-low (reverse (sort pvec))
         m (double (count pvec))]
     (loop
       [p (first sorted-hi-low)
        r (rest sorted-hi-low)
        k m]
       (if-not p
         :nothing-passes-fdr
         (if (<= p (* alpha (/ (double k) m)))
           p
           (recur (first r) (rest r) (dec k))))))))

