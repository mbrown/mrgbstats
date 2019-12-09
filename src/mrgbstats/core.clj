(ns mrgbstats.core
  (:require [mrgbstats.permtest :as permtest]
            [mrgbstats.atoms :as atoms]
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
  "Permutation testing. Tests for difference of single variable function of
  two groups. Single variable function could be mean, variance, or any other
  function taking n-samples of a single variable as input.
  a and b = seqs or vecs of numbers to compare
  func = single variable function (eg: mean, variance, etc.)
  numiter = number of iterations
  parallel? = Boolean (default false). If true, runs permutations on four parallel threads.
  If running in parallel mode, numiter should be a multiple of 4.
  Returns two-tailed p-value."
  ([a b func numiter]
   (permtest-singlevar a b func numiter false))
  ([a b func numiter parallel?]
   (if parallel?
     (permtest/permtest-singlevar-parallel a b func numiter)   
     (permtest/permtest-singlevar-serial a b func numiter))))

(defn permtest-multiprops
  "Permutation testing for multiple categorical variables based on chi
  squared. Tests for difference of distributions between two groups.
  a and b = seqs or vecs.
  numiter = number
  Returns p-value."
  ([a b numiter]
   (permtest-multiprops a b numiter false))
  ([a b numiter parallel?]
   (when
     (and parallel?
          (not @atoms/warning-given-no-parallel-permtest-multiprops-atom))
     (reset! atoms/warning-given-no-parallel-permtest-multiprops-atom true)
     (println
       "WARNING: parallel computation not implemented for permtest-multiprops fn,"
       "defaulting to single thread computation."))
   (permtest/permtest-multiprops-serial a b numiter)))

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

