(ns mrgbstats.permtest
  (:require [random-seed.core :as random-seed]))

; ----------
; Permutation testing to compared two groups on a single variable (eg: mean)

(defn permtest-singlevar-sim-diff
  "Simulates one iteration for permutation testing.
  Used by permtest-singlevar-absdiffs fn."
  [a b func]
  (let [[sima simb]
        (split-at (count a) (shuffle (concat a b)))]
    (- (func sima) (func simb))))

(defn permtest-singlevar-absdiffs
  "Used by permtest-singlevar-serial and permtest-singlevar-parallel fns.
  Returns vec of absolute diffs
  |func(simulated group a) - func(simulated group b)|"
  [a b func numiter]
  (random-seed/set-random-seed!
    (+ (.getId (Thread/currentThread))
       (Math/abs (.hashCode (java.time.Instant/now)))))
  (loop
    [diffvec (transient (vec (repeat numiter 0.0)))
     i (dec numiter)]
    (if (< i 0)
      (let [p (persistent! diffvec)]
        ;(println p)
        p)
      (recur
        (assoc! diffvec i (Math/abs (permtest-singlevar-sim-diff a b func)))
        (dec i)))))

(defn permtest-singlevar-serial
  "Permutation testing. Tests for difference of single variable function of
  two groups. Single variable function could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  Uses single thread. 
  a and b = seqs or vecs
  func = single variable function (eg: mean, variance, etc.)
  numiter = number of iterations
  Returns two-tailed p-value."
  [a b func numiter]
  (let
    [actual-abs-diff
     (Math/abs (- (func a) (func b)))
     abs-diffs
     (conj (permtest-singlevar-absdiffs a b func (dec numiter)) actual-abs-diff)]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter)))

(defn permtest-singlevar-parallel
  "Permutation testing. Tests for difference of single variable function of
  two groups. Single variable function could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  Uses four threads.
  a and b = seqs or vecs
  func = single variable function (eg: mean, variance, etc.)
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Returns two-tailed p-value."
  [a b func numiter]
  (let
    [actual-abs-diff
     (Math/abs (- (func a) (func b)))
     numiter-by-4 (/ numiter 4)
     abs-diffs
     (concat
       (deref (future (permtest-singlevar-absdiffs a b func numiter-by-4)))
       (deref (future (permtest-singlevar-absdiffs a b func numiter-by-4)))
       (deref (future (permtest-singlevar-absdiffs a b func numiter-by-4)))
       (deref (future (permtest-singlevar-absdiffs a b func (dec numiter-by-4))))
       [actual-abs-diff])]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter) ))

; ----------
; Permutation testing to compared two groups in terms of multiple proportions

(defn compute-refprops
  "Computes reference proportions of all values under null hypothesis
  that a, b are from same distribution."
  [a b]
  (let [c (concat a b)
        n (count c)
        valset (set c)]
    (into {} (for [v valset]
               [v (/ (double (count (filter #(= % v) c))) n)]))))
(defn sq
  [x]
  (* x x))

(defn chisq-multi
  "Chi squared computation for two samples of categorical variables.
  refprops = hash-map mapping values to proportions, must contain all values
  present in a and b, sum of proportions must be 1.0
  Used by permtest-multiprops-sim-chisq fn."
  [a b refprops]
  (let
    [valset (set (concat (set a) (set b)))
     q (for [v valset]
         (let [expected-a (double (* (count a) (get refprops v)))
               expected-b (double (* (count b) (get refprops v)))
               num-a (count (filter #(= % v) a))
               num-b (count (filter #(= % v) b))
               sq-a (/ (sq (- num-a expected-a)) expected-a)
               sq-b (/ (sq (- num-b expected-b)) expected-b) 
               ;sq-a (/ (sq (- num-a expected-a)) 1.0)
               ;sq-b (/ (sq (- num-b expected-b)) 1.0)
               ]
           (+ sq-a sq-b)))]
    (reduce + q)))

(defn permtest-multiprops-sim-chisq
  "Simulates one iteration for permutation testing of multiple categorical variables.
  Used by permtest-multiprops-vec-chisq fn."
  [a b refprops]
  ;(random-seed/set-random-seed!
    ;(+ (.getId (Thread/currentThread))
       ;(Math/abs (.hashCode (java.time.Instant/now)))))
  (let [[sima simb]
        (split-at (count a) (shuffle (concat a b)))]
    (chisq-multi sima simb refprops)))

(defn permtest-multiprops-vec-chisq
  "Used by permtest-multiprops-serial fn.
  Returns vec of chi squared values from simulated groups a, b"
  [a b numiter refprops]
  (random-seed/set-random-seed!
    (+ (.getId (Thread/currentThread))
       (Math/abs (.hashCode (java.time.Instant/now)))))
  (loop
    [chisqvec (transient (vec (repeat numiter 0.0)))
     i (dec numiter)]
    (if (< i 0)
      (persistent! chisqvec)
      (recur
        (assoc! chisqvec i (permtest-multiprops-sim-chisq a b refprops))
        (dec i)))))

(defn permtest-multiprops-serial
  "Permutation testing. Tests for difference of distributions of
  categorical variables in two groups using chi squared.
  a and b = seqs or vecs. Uses single thread.
  numiter = number
  Returns p-value."
  [a b numiter]
  (let [refprops (compute-refprops a b) 
        actual-chisq (chisq-multi a b refprops)
        chisqvec (conj (permtest-multiprops-vec-chisq a b (dec numiter) refprops) actual-chisq)]
    (/ (double (count (filter #(>= %1 actual-chisq) chisqvec)))
       numiter)))

