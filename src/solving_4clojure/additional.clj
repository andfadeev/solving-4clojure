(ns solving-4clojure.additional
  (:require [clojure.test :refer [is are]]))

;; simple sieve of Eratosthenes

(defn sieve-of-eratosthenes
  [n]
  (loop [res [1]
         coll (range 2 (inc n))]
    (let [prime (first coll)]
      (if (seq coll)
        (recur
          (conj res prime)
          (filter #(not= (mod % prime) 0) coll))
        res))))

(is (= (sieve-of-eratosthenes 10) [1 2 3 5 7]))