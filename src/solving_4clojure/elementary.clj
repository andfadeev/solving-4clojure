(ns solving-4clojure.elementary
  (:require [clojure.test :refer [is]]))

;; https://www.4clojure.com/problem/156

(defn to-map [q w]
  (reduce #(assoc % %2 q) {} w))

(is (= (to-map 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
(is (= (to-map "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
(is (= (to-map [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))
