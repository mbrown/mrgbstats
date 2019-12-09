(ns mrgbstats.core
  (:require [random-seed.core :as random-seed]))

(defn mean
  "a = seq or vec of numbers"
  [a]
  (/ (double (reduce + a)) (count a)))

(defn- permtest-sim-diff
  "Simulates one iteration for permutation testing.
  Used by permtest-absdiffs fn."
  [a b]
  (let [[sima simb]
        (split-at (count a) (shuffle (concat a b)))]
    (- (mean sima) (mean simb))))

(defn- permtest-absdiffs
  "Used by permtest-serial and permtest-parallel fns.
  Returns vec of absolute diffs |simulated group a - b|"
  [a b numiter]
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
        (assoc! diffvec i (Math/abs (permtest-sim-diff a b)))
        (dec i)))))

(defn- permtest-serial
  "Permutation testing. Tests for difference of means of two groups.
  Uses single thread. 
  a and b = seqs or vecs.
  numiter = number of iterations
  Returns two-tailed p-value."
  [a b numiter]
  (let [actual-abs-diff
        (Math/abs (- (mean a) (mean b)))
        abs-diffs
        (conj (permtest-absdiffs a b (dec numiter)) actual-abs-diff)]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter)))

(defn- permtest-parallel
  "Permutation testing. Tests for difference of means of two groups.
  Uses four threads.
  a and b = seqs or vecs.
  numiter = number of iterations, should be divisible by 4 (will work if not,
  but some results might be slightly off expected)
  Returns two-tailed p-value."
  [a b numiter]
  (let [actual-abs-diff
        (Math/abs (- (mean a) (mean b)))
        numiter-by-4 (/ numiter 4)
        abs-diffs
        (concat
          (deref (future (permtest-absdiffs a b numiter-by-4)))
          (deref (future (permtest-absdiffs a b numiter-by-4)))
          (deref (future (permtest-absdiffs a b numiter-by-4)))
          (deref (future (permtest-absdiffs a b (dec numiter-by-4))))
          [actual-abs-diff])]
    (/ (double (count (filter #(>= %1 actual-abs-diff) abs-diffs)))
       numiter) ))

(defn permtest
  "Permutation testing. Tests for difference of means of two groups.
  a and b = seqs or vecs of numbers to compare
  numiter = number of iterations
  parallel = Boolean (default false). If true, runs permutations on four parallel threads.
             If running in parallel mode, numiter should be a multiple of 4.
  Returns two-tailed p-value."
  ([a b numiter]
   (permtest a b numiter false))
  ([a b numiter parallel]
   (if parallel
     (permtest-parallel a b numiter)   
     (permtest-serial a b numiter))))

(defn fdr
  "pvec = vec of p-values
  alpha = corrected p-value (default 0.05)
  Performs false discovery rate (FDR) correction for multiple comparisons
  using the Benjamini-Hochberg method.
  Returns p threshold or :nothing-passes-fdr if nothing passes FDR
  correction."
  ([pvec]
   (fdr pvec 0.05))
  ([pvec alpha]
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

