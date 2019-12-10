(ns mrgbstats.core-test
  (:require [clojure.test :refer :all]
            [mrgbstats.core :refer :all]))

(def printret-on false)

(defn printret
  "Prints x then returns it"
  ([x]
   (printret nil x))
  ([msg x]
   (if printret-on
     (if msg
       (println msg x)
       (println x)))
   x))

(deftest test-permtest-singlevar-mean-groups-identical
  (testing
    "Testing permtest-singlevar, mean, groups identical, should have p=1.0."
    (is (= (permtest-singlevar
             [0 1 2 3 4 5]
             [0 1 2 3 4 5]
             mean mean 200 nil)
           1.0))))

(deftest test-permtest-singlevar-mean-groups-different
  (testing
    "Testing permtest-singlevar, mean, groups different, should have p<0.01."
    (is (< (permtest-singlevar
             [20 20 21 21 22 22 23 23 24 24 25 25 26 26]
             [0 0 1 1 2 2 3 3 4 4 5 5 6 6]
             mean mean 200 nil)
           0.01))))

(deftest test-permtest-singlevar-variance-groups-identical
  (testing
    "Testing permtest-singlevar, variance, groups identical, should have
    p=1.0."
    (is (= (permtest-singlevar
             [0 1 2 3 4 5 6]
             [0 1 2 3 4 5 6]
             variance variance 200 nil)
           1.0))))

(deftest test-permtest-singlevar-variance-groups-different
  (testing
    "Testing permtest-singlevar, variance, groups different, should have
    p<0.01."
    (is (< (permtest-singlevar
             [0 1 2 3 3 3 3 3 3 4 5 6]
             [0 0 0 0 0 0 6 6 6 6 6 6]
             variance variance 200 nil)
           0.01))))

(deftest test-permtest-mean-groups-identical
  (testing
    "Testing permtest-mean, groups identical, should have p=0.1."
    (is (= (printret
             "permtest-mean, grups same, p ="
             (permtest-mean
               [0 1 2 3 4 5]
               [0 1 2 3 4 5]
               200 nil))
           1.0))))

(deftest test-permtest-mean-groups-different
  (testing
    "permtest-mean, groups different, should have p<0.01."
    (is (< (permtest-mean
             [20 20 21 21 22 22 23 23 24 24 25 25 26 26]
             [0 0 1 1 2 2 3 3 4 4 5 5 6 6]
             200 nil)
           0.01))))

(deftest test-permtest-linear-slope-no-effect
  (testing
    "permtest-linear-slope, no effect, should have p=0.1."
    (is (= (printret
             "permtest-linear-slope, no effect present, p ="
             (permtest-linear-slope
               [0 0 0 0 1 1 1 1 2 2 2 2]
               [1 2 3 4 1 2 3 4 1 2 3 4]
               200 nil))
           1.0))))

(deftest test-permtest-linear-slope-effect-present
  (testing
    "permtest-linear-slope, effect present, should have p<0.01."
    (is (< (printret
             "permtest-linear-slope, effect present, p ="
             (permtest-linear-slope
               [0 0 0 0 1 1 1 1 2 2 2 2]
               [10 10 10 10 11 11 11 11 12 12 12 12]
               200 nil))
           0.01))))

(deftest test-permtest-linear-slope-weak-effect
  (testing
    "permtest-linear-slope, weak effect, should have p>0.05."
    (is (> (printret
             "permtest-linear-slope, weak effect present, p ="
             (permtest-linear-slope
               [0 0 0 0 1 1 1 1 2 2 2 2]
               [1 2 2 3 2 2 3 3 2 3 3 4]
               200 nil))
           0.05))))

