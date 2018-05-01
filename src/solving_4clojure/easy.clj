(ns solving-4clojure.easy
  (:require [clojure.test :refer [is are]])
  (:import java.lang.Character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/19
;; Last Element
;; Write a function which returns the last element in a sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-last [s]
  (loop [s s]
    (if (next s)
      (recur (next s))
      (first s))))

(= (my-last [1 2 3 4 5]) 5)
(= (my-last '(5 4 3)) 3)
(= (my-last ["b" "c" "d"]) "d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/23
;; Reverse a Sequence
;; Write a function which reverses a sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-reverse-seq [s]
  (loop [ret []
         s s]
    (if-not (empty? s)
      (recur (conj ret (last s)) (butlast s))
      (seq ret))))

(= (my-reverse-seq [1 2 3 4 5]) [5 4 3 2 1])
(= (my-reverse-seq (sorted-set 5 7 2 7)) '(7 5 2))
(= (my-reverse-seq [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

(defn some-fn []
  (println "some-fn"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/26
;; Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fibonacci [x]
  (take x (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(= (fibonacci 3) '(1 1 2))
(= (fibonacci 3) '(1 1 2))
(= (fibonacci 6) '(1 1 2 3 5 8))
(= (fibonacci 8) '(1 1 2 3 5 8 13 21))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/28
;; Flatten a Sequence
;; Write a function which flattens a sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-flat? [col]
  (empty? (filter sequential? col)))

(defn my-flatten [s]
  (loop [s s]
    (if (is-flat? s)
      s
      (recur (mapcat #(if (sequential? %) % [%]) s)))))

(= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c"))
(= (my-flatten '((((:a))))) '(:a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/29
;; Get the Caps
;; Write a function which takes a string and returns a new string
;; containing only the capital letters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn only-uppercase [s]
  (apply str (filter #(Character/isUpperCase %) s)))

(= (only-uppercase "HeLlO, WoRlD!") "HLOWRD")
(empty? (only-uppercase "nothing"))
(= (only-uppercase "$#A(*&987Zf") "AZ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/30
;; Compress a Sequence
;; Write a function which removes consecutive duplicates from a sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-duplicates [s]
  (loop [result []
         s (seq s)]
    (if (empty? s)
      result
      (if (= (last result) (first s))
        (recur result (rest s))
        (recur (conj result (first s)) (rest s))))))

(= (apply str (remove-duplicates "Leeeeeerrroyyy")) "Leroy")
(= (remove-duplicates [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (remove-duplicates [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/31
;; Pack a Sequence
;; Write a function which packs consecutive duplicates into sub-lists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pack-a-sequence [s]
  (partition-by identity s))

(is (= (pack-a-sequence [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
(is (= (pack-a-sequence [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
(is (= (pack-a-sequence [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/32
;; Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dublicate-seq [col]
  (apply concat '() (map #(vector % %) col)))

(is (= (dublicate-seq [1 2 3]) '(1 1 2 2 3 3)))
(is (= (dublicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
(is (= (dublicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
(is (= (dublicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

;; https://www.4clojure.com/problem/33

(defn replicate-seq [col n]
  (mapcat #(repeat n %) col))

(is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
(is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
(is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
(is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
(is (= (replicate-seq [4 5 6] 1) '(4 5 6)))
(is (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
(is (= (replicate-seq [44 33] 2) [44 44 33 33]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/34
;; Implement range
;; Write a function which creates a list of all integers in a given range.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-range [from to]
  (loop [i from
         result []]
    (if (< i to)
      (recur (inc i) (conj result i))
      (seq result))))

(= (my-range 1 4) '(1 2 3))
(= (my-range -2 2) '(-2 -1 0 1))
(= (my-range 5 8) '(5 6 7))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/38
;; Maximum value
;; Write a function which takes a variable number of parameters and
;; returns the maximum value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-max [& col] (-> col sort last))

(= (my-max 1 8 3 4) 8)
(= (my-max 30 20) 30)
(= (my-max 45 67 11) 67)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/39
;; Interleave Two Seqs
;; Write a function which takes two sequences and returns the first
;; item from each, then the second item from each, then the third, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-interleave [& cols]
  (flatten (apply map list cols)))

(= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
(= (my-interleave [1 2 3 4] [5]) [1 5])
(= (my-interleave [30 20] [25 15]) [30 25 20 15])
(= (my-interleave [30 20] [25 15]) [30 25 20 15])

;; https://www.4clojure.com/problem/41

(defn filter-by-index [col n]
  (keep-indexed #(if (not= 0 (mod (inc %) n))
                   %2
                   nil) col))

(is (= (filter-by-index [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
(is (= (filter-by-index [:a :b :c :d :e :f] 2) [:a :c :e]))
(is (= (filter-by-index [1 2 3 4 5 6] 4) [1 2 3 5 6]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/42
;; Factorial Fun
;; Write a function which calculates factorials.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn my-factorial [n]
  (apply *' (range 1 (inc n))))

(= (my-factorial 1) 1)
(= (my-factorial 3) 6)
(= (my-factorial 5) 120)
(= (my-factorial 8) 40320)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-implement Iterate
;; https://www.4clojure.com/problem/62
;; Given a side-effect free function f and an initial value x write a function
;; which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; solution
(defn my-iterate [f x]
  (cons x (lazy-seq (my-iterate f (f x)))))

;; or we can user fn form with name to recur
;; (fn my-iterate [f x]
;;   (cons x (lazy-seq (my-iterate f (f x)))))

;; tests
(= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (my-iterate inc 0)) (take 100 (range)))
(= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))


;; https://www.4clojure.com/problem/63

(defn my-group-by [f c]
  (reduce (fn [res el]
            (let [k (first el)
                  v (second el)]
              (if (contains? res k)
                (assoc res k (conj (get res k) v))
                (assoc res k (vector v)))))
          {}
          (map #(vector (f %) %) c)))

(is (= (my-group-by #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
(is (= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
       {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
(is (= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
       {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))

;; http://www.4clojure.com/problem/66
(def problem-66
  (fn [a b]
    (cond
      (< a b) (recur b a)
      (zero? b) a
      :else (recur b (mod a b)))))

(is (= (problem-66 2 4) 2))
(is (= (problem-66 10 5) 5))
(is (= (problem-66 5 7) 1))
(is (= (problem-66 1023 858) 33))

;; http://www.4clojure.com/problem/76
(def problem-76 (range 1 12 2))
(is (= problem-76
       (letfn
         [(foo [x y] #(bar (conj x y) y))
          (bar [x y] (if (> (last x) 10)
                       x
                       #(foo x (+ 2 y))))]
         (trampoline foo [] 1))))

;; https://www.4clojure.com/problem/81
(defn intersect [s1 s2]
  (set (filter s1 s2)))

(is (= (intersect #{0 1 2 3} #{2 3 4 5}) #{2 3}))

;; https://www.4clojure.com/problem/83
(defn half-truth [& args]
  (if (every? true? args)
    false
    (if (some true? args) true false)))

(is (= false (half-truth false false)))

;; https://www.4clojure.com/problem/88
(defn sym-diff [s1 s2]
  (into (set (filter (fn [e] (not (s1 e))) s2))
        (filter (fn [e] (not (s2 e))) s1)))

;; https://www.4clojure.com/problem/90
(defn cartesian [s1 s2]
  (set (mapcat (fn [e] (map (fn [z] [z e]) s1)) s2)))

(is (= (cartesian #{1 2 3} #{4 5})
       #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))

;; https://www.4clojure.com/problem/95
(defn is-tree? [coll]
  (boolean (when (and (sequential? coll) (seq coll) (= 3 (count coll)))
             (let [[root l r] coll]
               (println root l r)
               (and (not (nil? root))
                    (or (nil? l)
                        (is-tree? l))
                    (or (nil? r)
                        (is-tree? r)))))))

;; http://www.4clojure.com/problem/96
(def problem-96
  (fn [tree]
    (letfn [(root [coll] (first coll))
            (left-child [coll] (second coll))
            (right-child [coll] (last coll))
            (symmetric? [left-sub-tree
                         right-sub-tree]
              (if (and (sequential? left-sub-tree)
                       (sequential? right-sub-tree))
                (if (= (root left-sub-tree)
                       (root right-sub-tree))
                  (and (symmetric? (left-child left-sub-tree)
                                   (right-child right-sub-tree))
                       (symmetric? (right-child left-sub-tree)
                                   (left-child right-sub-tree)))
                  false)
                (= left-sub-tree right-sub-tree)))]
      (symmetric? (second tree) (last tree)))))

(is (= (problem-96 '(:a (:b nil nil) (:b nil nil))) true))
(is (= (problem-96 '(:a (:b nil nil) nil)) false))
(is (= (problem-96 '(:a (:b nil nil) (:c nil nil))) false))
(is (= (problem-96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                    [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true))
(is (= (problem-96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                    [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]]) false))
(is (= (problem-96 [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                    [2 [3 nil [4 [6 nil nil] nil]] nil]]) false))

;; https://www.4clojure.com/problem/97
(defn pascal-triangle-row [nth]
  (case nth
    1 [1]
    2 [1 1]
    (let [row (pascal-triangle-row (dec nth))]
      (into [] (concat [1] (map + (rest row) (drop-last row)) [1])))))


;; https://www.4clojure.com/problem/99
(defn mult-and-seq [x y]
  (into [] (map #(bigint (str %)) (str (* x y)))))

;; http://www.4clojure.com/problem/100
(def problem-100
  (fn problem-100
    ([a b]
      (letfn [(gcm [a b]
                (cond
                  (< a b) (recur b a)
                  (zero? b) a
                  :else (recur b (mod a b)))
                )]
        (/ (* a b) (gcm a b))))
    ([a b & r]
      (apply problem-100 (problem-100 a b) r))))

(== (problem-100 2 3) 6)
(== (problem-100 5 3 7) 105)
(== (problem-100 1/3 2/5) 2)
(== (problem-100 3/4 1/6) 3/2)
(== (problem-100 7 5/7 2 3/5) 210)

;; https://www.4clojure.com/problem/118
(defn my-map [f col]
  (lazy-seq
   (when-let [s (seq col)]
     (cons (f (first s)) (my-map f (rest s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.4clojure.com/problem/120
;; Sum of square of digits
;; Write a function which takes a collection of integers as an argument.
;; Return the count of how many elements are smaller than the sum of their
;; squared component digits. For example: 10 is larger than 1 squared plus 0 squared;
;; whereas 15 is smaller than 1 squared plus 5 squared.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn filter-with-sum-of-squares [col]
  (count
   (filter
    (fn [i]
      (> (apply + (map (fn [x] (* x x)) (map #(Integer/parseInt (str %)) (seq (str i))))) i))
    col)))

(is (= 8 (filter-with-sum-of-squares (range 10))))
(is (= 19 (filter-with-sum-of-squares (range 30))))
(is (= 50 (filter-with-sum-of-squares (range 100))))
(is (= 50 (filter-with-sum-of-squares (range 1000))))

;; https://www.4clojure.com/problem/122
(defn parse-binary [s]
  (Integer/parseInt s 2))

;; http://www.4clojure.com/problem/128
(def problem-128
  (fn [[suit rank]]
    (let [suit-map {\S :spade
                    \H :heart
                    \D :diamond
                    \C :club}
          rank-map {\2 2
                    \3 3
                    \4 4
                    \5 5
                    \6 6
                    \7 7
                    \8 8
                    \9 9
                    \T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 14}]
      {:suit (get suit-map suit)
       :rank (- (get rank-map rank) 2)})))

(= {:suit :diamond :rank 10} (problem-128 "DQ"))
(= {:suit :heart :rank 3} (problem-128 "H5"))
(= {:suit :club :rank 12} (problem-128 "CA"))
(= (range 13) (map (comp :rank problem-128 str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))

;; https://www.4clojure.com/problem/135
(defn infix-calc [& args]
  (reduce (fn [i [f val]] (f i val)) (first args) (partition 2 (rest args))))

;; http://www.4clojure.com/problem/146
(def problem-146
  (fn [input]
    (->> (for [[k v] input
               [k2 v2] v]
           {[k k2] v2})
         (apply merge))))

(= (problem-146 '{a {p 1, q 2}
                  b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})
(= (problem-146 '{[1] {a b c d}
         [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})
(= (problem-146 '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})

;; http://www.4clojure.com/problem/147
(def problem-147
  (fn problem-147 [vec]
    (lazy-seq
      (cons vec
            (problem-147
              (let [middle
                    (loop [out []
                           coll vec]
                      (if (> (count coll) 1)
                        (recur (conj out (+' (first coll)
                                             (second coll)))
                               (rest coll))
                        out))]
                (into [] (concat [(first vec)] middle [(last vec)]))))))))

(= (second (problem-147 [2 3 2])) [2 5 5 2])
(= (take 5 (problem-147 [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (problem-147 [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (problem-147 [2 4 2])) (rest (take 101 (problem-147 [2 2]))))

;; http://www.4clojure.com/problem/153
(def problem-153
  (fn [set-of-sets]
    (->> (for [current-set set-of-sets
               other-set set-of-sets]
           (when (not (= current-set other-set))
             (boolean (seq (clojure.set/intersection current-set other-set)))))
         (some true?)
         (not))))

(is (= (problem-153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}) true))
(is (= (problem-153 #{#{:a :b :c :d :e}
                      #{:a :b :c :d}
                      #{:a :b :c}
                      #{:a :b}
                      #{:a}}) false))
(is (= (problem-153 #{#{[1 2 3] [4 5]}
                      #{[1 2] [3 4 5]}
                      #{[1] [2] 3 4 5}
                      #{1 2 [3 4] [5]}}) true))

;; https://www.4clojure.com/problem/157
(defn index-seq [s]
  (map-indexed (fn [i e] [e i]) s))

;; http://www.4clojure.com/problem/166
(def problem-166
  (fn [less-then-operator x y]
    (let [x-less-then-y (less-then-operator x y)]
      (if x-less-then-y
        :lt
        (let [y-less-then-x (less-then-operator y x)]
          (if y-less-then-x
            :gt
            :eq))))))

(= :gt (problem-166 < 5 1))
(= :eq (problem-166 (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (problem-166 (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (problem-166 > 0 2))
