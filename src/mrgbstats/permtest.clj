(ns mrgbstats.permtest
  (:require [random-seed.core :as random-seed]))

; ----------
; Permutation testing to compare two groups on a single variable (eg: mean)

(defn permtest-singlevar-sim-diff
  "Simulates one iteration for permutation testing.
  Used by permtest-singlevar-absdiffs fn.
  a and b = seqs or vecs
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively."
  [a b func-a func-b]
  (let [[sima simb]
        (split-at (count a) (shuffle (concat a b)))]
    (- (func-a sima) (func-b simb))))

(defn permtest-singlevar-absdiffs
  "Used by permtest-singlevar-serial and permtest-singlevar-parallel fns.
  a and b = seqs or vecs
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively.
  numiter = number of iterations
  Returns vec of absolute diffs
  |func(simulated group a) - func(simulated group b)|"
  [a b func-a func-b numiter]
  (random-seed/set-random-seed!
    (+ (.getId (Thread/currentThread))
       (Math/abs (.hashCode (java.time.Instant/now)))))
  (loop
    [diffvec (transient (vec (repeat numiter 0.0)))
     i (dec numiter)]
    (if (< i 0)
      (persistent! diffvec)
      (recur
        (assoc! diffvec i
                (Math/abs (permtest-singlevar-sim-diff a b func-a func-b)))
        (dec i)))))

(defn permtest-singlevar-serial
  "Permutation testing. Tests for difference of single variable function of
  two groups. Single variable function could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  Uses single thread. 
  a and b = seqs or vecs
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively.
  numiter = number of iterations
  Returns two-tailed p-value."
  [a b func-a func-b numiter]
  (let
    [actual-abs-diff
     (Math/abs (- (func-a a) (func-b b)))
     abs-diffs
     (conj (permtest-singlevar-absdiffs a b func-a func-b (dec numiter))
           actual-abs-diff)]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter)))

(defn permtest-singlevar-parallel
  "Permutation testing. Tests for difference of single variable function of
  two groups. Single variable function could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  Uses four threads.
  a and b = seqs or vecs
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively.
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Returns two-tailed p-value."
  [a b func-a func-b numiter]
  (let
    [actual-abs-diff
     (Math/abs (- (func-a a) (func-b b)))
     numiter-by-4 (/ numiter 4)
     abs-diffs
     (concat
       (deref
         (future
           (permtest-singlevar-absdiffs a b func-a func-b numiter-by-4)))
       (deref
         (future
           (permtest-singlevar-absdiffs a b func-a func-b numiter-by-4)))
       (deref
         (future
           (permtest-singlevar-absdiffs a b func-a func-b numiter-by-4)))
       (deref
         (future
           (permtest-singlevar-absdiffs a b func-a func-b
                                        (dec numiter-by-4))))
       [actual-abs-diff])]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter) ))

(defn permtest-singlevar
  "Permutation testing. Tests for difference of single variable functions of
  two groups. Single variable functions could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  a and b = seqs or vecs of numbers to compare
  func-a and func-b = single variable functions (eg: mean, variance, etc.)
  applied to inputs a and b, respectively.
  numiter = number of iterations
  last argument = options hash-map, possible keys :parallel?
    parallel? = Boolean (default true). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns two-tailed p-value."
  [a b func-a func-b numiter {:keys [parallel?]}]
  (if (not parallel?)
    (permtest-singlevar-serial a b func-a func-b numiter)  
    (permtest-singlevar-parallel a b func-a func-b numiter)))

; ----------
; Permutation testing to compare two groups in terms of multiple proportions
; (categories)

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
  "Permutation testing. Tests for difference of distributions of categorical
  variables in two groups using chi squared.
  Uses single thread. 
  a and b = seqs or vecs.
  numiter = number
  Returns p-value."
  [a b numiter]
  (let [refprops (compute-refprops a b) 
        actual-chisq (chisq-multi a b refprops)
        chisqvec (conj (permtest-multiprops-vec-chisq a b (dec numiter) refprops)
                       actual-chisq)]
    (/ (double (count (filter #(>= %1 actual-chisq) chisqvec))) numiter)))

(defn permtest-multiprops-parallel
  "Permutation testing. Tests for difference of distributions of categorical
  variables in two groups using chi squared.
  Uses four threads.
  a and b = seqs or vecs
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Returns p-value."
  [a b numiter]
  (let
    [refprops (compute-refprops a b) 
     actual-chisq (chisq-multi a b refprops)
     numiter-by-4 (/ numiter 4)
     chisqvec
     (concat
       (deref
         (future
           (permtest-multiprops-vec-chisq a b numiter-by-4 refprops)))
       (deref
         (future
           (permtest-multiprops-vec-chisq a b numiter-by-4 refprops)))
       (deref
         (future
           (permtest-multiprops-vec-chisq a b numiter-by-4 refprops)))
       (deref
         (future
           (permtest-multiprops-vec-chisq a b (dec numiter-by-4) refprops)))
       [actual-chisq])]
    (/ (double (count (filter #(>= %1 actual-chisq) chisqvec))) numiter)))

(defn permtest-multiprops
  "Permutation testing for multiple categorical variables based on chi
  squared. Tests for difference of distributions between two groups.
  a and b = seqs or vecs.
  numiter = number
  last argument = options hash-map, possible keys :parallel?
    parallel? = Boolean (default true). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns p-value."
  [a b numiter {:keys [parallel?]}]
  (if (not parallel?)
    (permtest-multiprops-serial a b numiter)  
    (permtest-multiprops-parallel a b numiter)))

; ----------
; Permutation testing to compare single general linear model (GLM) regression
; parameter vs. zero
; linear model: y = M*X, where M = GLM design matrix. 
; eg: y = slope * x + intercept.

(defn permtest-oneglmparam-absvals
  "Used by permtest-oneglmparam-serial and permtest-oneglmparam-parallel fns.
  y = seq or vec of dependent variables (which are permuted)
  glm-func = function that returns one parameter from fitted GLM
  glm-func is assumed to close over x (independent variables, not permuted)
  used in linear model y = M*X, where M = GLM design matrix. See mrgbstats.glm. 
  numiter = number of iterations
  Returns vec of absolute parameter values."
  [y glm-func numiter]
  (random-seed/set-random-seed!
    (+ (.getId (Thread/currentThread))
       (Math/abs (.hashCode (java.time.Instant/now)))))
  (loop
    [valsvec (transient (vec (repeat numiter 0.0)))
     i (dec numiter)]
    (if (< i 0)
      (persistent! valsvec)
      (recur
        (assoc! valsvec i (Math/abs (glm-func (shuffle y))))
        (dec i)))))

(defn permtest-oneglmparam-serial
  "Permutation testing. Compares single general linear model (GLM) regression
  parameter vs. zero.
  linear model: y = M*X, where M = GLM design matrix. 
  eg: y = slope * x + intercept.
  y = seq or vec of dependent variables (which are permuted)
  glm-func = function that returns one parameter from fitted GLM
  glm-func is assumed to close over x (independent variables, not permuted)
  used in linear model y = M*X, where M = GLM design matrix. See mrgbstats.glm. 
  numiter = number of iterations
  Uses single thread. 
  Returns two-tailed p-value."
  [y glm-func numiter]
  (let
    [actual-abs-val (Math/abs (glm-func y))
     abs-vals
     (conj (permtest-oneglmparam-absvals y glm-func (dec numiter))
           actual-abs-val)]
    (/ (double (count (filter #(>= %1 actual-abs-val) abs-vals)))
       numiter)))

(defn permtest-oneglmparam-parallel
  "Permutation testing. Compares single general linear model (GLM) regression
  parameter vs. zero.
  linear model: y = M*X, where M = GLM design matrix. 
  eg: y = slope * x + intercept.
  y = seq or vec of dependent variables (which are permuted)
  glm-func = function that returns one parameter from fitted GLM
  glm-func is assumed to close over x (independent variables, not permuted)
  used in linear model y = M*X, where M = GLM design matrix. See mrgbstats.glm. 
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Uses four threads. 
  Returns two-tailed p-value."
  [y glm-func numiter]
  (let
    [actual-abs-val (Math/abs (glm-func y))
     numiter-by-4 (/ numiter 4)
     abs-vals
     (concat
       (deref
         (future
           (permtest-oneglmparam-absvals y glm-func numiter-by-4)))
       (deref
         (future
           (permtest-oneglmparam-absvals y glm-func numiter-by-4)))
       (deref
         (future
           (permtest-oneglmparam-absvals y glm-func numiter-by-4)))
       (deref
         (future
           (permtest-oneglmparam-absvals y glm-func (dec numiter-by-4))))
       [actual-abs-val])]
    (/ (double (count (filter #(>= %1 actual-abs-val) abs-vals)))
       numiter) ))

(defn permtest-oneglmparam
  "Permutation testing. Compares single general linear model (GLM) regression
  parameter vs. zero.
  linear model: y = M*X, where M = GLM design matrix. 
  eg: y = slope * x + intercept.
  y = seq or vec of dependent variables (which are permuted)
  glm-func = function that returns one parameter from fitted GLM
  glm-func is assumed to close over x (independent variables, not permuted)
  used in linear model y = M*X, where M = GLM design matrix. See mrgbstats.glm. 
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Uses four threads. 
  last argument = options hash-map, possible keys :parallel?
    parallel? = Boolean (default true). If true, runs permutations on four
    parallel threads. If running in parallel mode, numiter should be a multiple
    of 4.
  Returns two-tailed p-value."
  [y glm-func numiter {:keys [parallel?]}]
  (if (not parallel?)
    (permtest-oneglmparam-serial y glm-func numiter)  
    (permtest-oneglmparam-parallel y glm-func numiter)))

; ----------
; Permutation testing to compare two groups with linear regression


