(ns solving-4clojure.medium
  (:require [clojure.test :refer [is are]]))

;; http://www.4clojure.com/problem/43
(def problem-43
  (fn [coll n]
    (loop [coll coll
           seqs (repeat n (list))]
      (if (seq coll)
        (recur (drop n coll)
               (map concat seqs (map list (take n coll))))
        seqs))))

;; much more elegant solution
(def problem-43*
  (fn [coll n]
    (apply map list (partition n coll))))

(doseq [solution [problem-43 problem-43*]]
  (is (= (solution [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (is (= (solution (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (is (= (solution (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

;; http://www.4clojure.com/problem/44
(def problem-44
  (fn [n col]
    (let [size (count col)
          offset (if (neg? n)
                   (- size (mod (- n) size))
                   (mod n size))]
      (take size (drop offset (cycle col))))))

(is (= (problem-44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(is (= (problem-44 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(is (= (problem-44 -2 [1 2 3 4 5]) '(4 5 1 2 3)))
(is (= (problem-44 6 [1 2 3 4 5]) '(2 3 4 5 1)))
(is (= (problem-44 1 '(:a :b :c)) '(:b :c :a)))
(is (= (problem-44 -4 '(:a :b :c)) '(:c :a :b)))

;; http://www.4clojure.com/problem/46
(def problem-46
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(is (= 3 ((problem-46 nth) 2 [1 2 3 4 5])))
(is (= true ((problem-46 >) 7 8)))
(is (= 4 ((problem-46 quot) 2 8)))
(is (= [1 2 3] ((problem-46 take) [1 2 3 4 5] 3)))

;; https://www.4clojure.com/problem/50
(defn group-by-type [coll]
  (vals (group-by type coll)))

;; https://www.4clojure.com/problem/54
(defn my-partition [n col]
  (lazy-seq
   (when-let [s (seq col)]
     (when (> (count s) n)
       (cons (take n col) (my-partition n (drop n col)))))))



;; https://www.4clojure.com/problem/55
(defn count-occur [coll]
  (apply merge (->> coll
                    sort
                    (partition-by identity)
                    (map (fn [gr] {(first gr) (count gr)}))
                    )))

;; https://www.4clojure.com/problem/56
(defn my-distinct
  ([col] (my-distinct col #{}))
  ([col pred]
   (filter #(not (nil? %))
           (lazy-seq
            (when-let [s (seq col)]
              (cons (when-not (contains? pred (first s)) (first s))
                    (my-distinct (rest s) (into pred [(first s)]))))))))

;; https://www.4clojure.com/problem/58
(defn my-comp [& funcs]
  (fn [& args]
    (reduce (fn [result next-fn]
              (println result next-fn)
              (next-fn result)) (apply (last funcs) args) (reverse (butlast funcs)))))

;; https://www.4clojure.com/problem/59
(defn my-juxt [& fs]
  (fn [& args]
    (reduce (fn [result next-fn] (conj result (apply next-fn args))) [] fs)))

;; http://www.4clojure.com/problem/60
(def problem-60
  (fn reductions*
    ([f val coll]
     (lazy-seq
       (if (seq coll)
         (let [res (f val (first coll))]
           (cons val (reductions* f res (rest coll))))
         (list val))))
    ([f coll]
     (reductions* f (first coll) (rest coll)))))

(is (= (take 5 (problem-60 + (range))) [0 1 3 6 10]))
(is (= (problem-60 conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
(is (= (last (problem-60 * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))

;; http://www.4clojure.com/problem/65
(def problem-65
  (fn [coll]
    (let [element [::some-key ::some-val]
          coll* (conj coll element element)]
      (if (= (count coll*) (inc (count coll)))
        (if (= ::some-val (get coll* ::some-key))
          :map
          :set)
        (let [coll* (-> coll
                        (conj ::first)
                        (conj ::second))]
          (if (= (first coll*) ::second)
            :list
            :vector))))))

(is (= :map (problem-65 {:a 1, :b 2})))
(is (= :set (problem-65 #{10 (rand-int 5)})))
(is (= :list (problem-65 (range (rand-int 20)))))
(is (= :vector (problem-65 [1 2 3 4 5 6])))
(is (= [:map :set :vector :list] (map problem-65 [{} #{} [] ()])))

;; http://www.4clojure.com/problem/67
(def problem-67
  (fn [n]
    (letfn [(is-prime
              [x]
              (loop [i 2]
                (if (> (* i i) x)
                  true
                  (if (= (mod x i) 0)
                    false
                    (recur (inc i))))))]
      (loop [res []
             x 2]
        (if (= (count res) n)
          res
          (if (is-prime x)
            (recur (conj res x) (inc x))
            (recur res (inc x))))))))

(is (= (problem-67 2) [2 3]))
(is (= (problem-67 5) [2 3 5 7 11]))
(is (= (last (problem-67 100)) 541))

;; https://www.4clojure.com/problem/69
(defn my-merge-with [merge-fn & maps]
  (reduce (fn [result next]
            (if (contains? result (first next))
              (update-in result [(first next)] merge-fn (second next))
              (assoc result (first next) (second next))))
          {} (mapcat seq maps)))

;; https://www.4clojure.com/problem/70
(defn word-sort [s]
  (sort-by clojure.string/lower-case (re-seq #"\w+" s)))

;; http://www.4clojure.com/problem/74
(def problem-74
  (fn [s]
    (letfn [(search
              [coll x]
              (loop [coll coll]
                (if (= (first coll) x)
                  x
                  (if (< (first coll) x)
                    (recur (rest coll))))))
            (squares
              [x]
              (lazy-seq
                (cons (* x x) (squares (inc x)))))]
      (let [squares-seq (squares 1)
            ints (->> (clojure.string/split s #",")
                      (map #(Integer. %)))]

        (->> (keep (partial search squares-seq) ints)
             (clojure.string/join ","))))))

(is (= (problem-74 "4,5,6,7,8,9") "4,9"))
(is (= (problem-74 "15,16,25,36,37") "16,25,36"))

;; http://www.4clojure.com/problem/75
(def problem-75
  (fn [x]
    (if (= x 1)
      1
      (letfn [(gcd [a b]
                (cond
                  (> b a) (recur b a)
                  (= b 0) a
                  :else (recur b (mod a b))))]
        (->> (range 1 x)
             (filter (fn [i] (= 1 (gcd x i))))
             (count))))))


(is (= (problem-75 1) 1))
(is (= (problem-75 10) (count '(1 3 7 9)) 4))
(is (= (problem-75 40) 16))
(is (= (problem-75 99) 60))

;; https://www.4clojure.com/problem/76
(= (range 1 12 2)
   (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; https://www.4clojure.com/problem/77
(defn find-anagram [col]
  (set (map set (filter #(> (count %) 1) (vals (group-by (fn [el] [(count el) (set el)]) col))))))

;; http://www.4clojure.com/problem/78
(def problem-78
  (fn problem-78
    ([f]
      (let [res (f)]
        (if (fn? res)
          (recur res)
          res)))
    ([f & args]
      (problem-78 #(apply f args)))))

(is (= (letfn [(triple [x] #(sub-two (* 3 x)))
               (sub-two [x] #(stop? (- x 2)))
               (stop? [x] (if (> x 50) x #(triple x)))]
         (problem-78 triple 2))
       82))
(is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
               (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
         (map (partial problem-78 my-even?) (range 6)))
       [true false true false true false]))

;; http://www.4clojure.com/problem/80
(def problem-80
  (fn [n]
    (loop [divisors-sum 1
           i 2]
      (if (< i n)
        (if (= (mod n i) 0)
          (recur (+ divisors-sum i) (inc i))
          (recur divisors-sum (inc i)))
        (= divisors-sum n)))))

(is (= (problem-80 6) true))
(is (= (problem-80 7) false))
(is (= (problem-80 496) true))
(is (= (problem-80 500) false))
(is (= (problem-80 8128) true))

;; http://www.4clojure.com/problem/85
(def problem-85
  (fn [original-set]
    ;; this solution can use memoization for sub-sets function to reduce time complexity, but it is too verbose
    (letfn [(sub-sets
              [n]
              (cond
                (= n 0) #{#{}}
                (= n 1) (->> (partition 1 original-set)
                             (map set)
                             (set))
                (= n (count original-set)) #{original-set}
                :else (let [old-sub-sets (sub-sets (dec n))]
                        (loop [sub-sets #{}
                               original-set original-set]
                          (if (seq original-set)
                            (let [element (first original-set)]
                              (recur (into sub-sets
                                           (->> old-sub-sets
                                                (map (fn [sub-set] (conj sub-set element)))
                                                (filter (fn [sub-set] (= (count sub-set) n)))))
                                     (rest original-set)))
                            sub-sets)))))]
      (reduce (fn [out n]
                (into out (sub-sets n)))
              #{} (range 0 (inc (count original-set)))))))

(is (= (problem-85 #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
(is (= (problem-85 #{}) #{#{}}))
(= (problem-85 #{1 2 3}) #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(is (= (count (problem-85 (into #{} (range 10)))) 1024))

;; http://www.4clojure.com/problem/86
(def problem-86
  (fn [x]
    (loop [retries 1000
           x x]
      (if (> retries 0)
        (let [sum-of-squares
              (->> (str x)
                   (map #(Integer. (str %)))
                   (map #(* % %))
                   (apply +))]
          (if (= sum-of-squares 1)
            true
            (recur (dec retries) sum-of-squares)))
        false))))

(is (= (problem-86 7) true))
(is (= (problem-86 986543210) true))
(is (= (problem-86 2) false))
(is (= (problem-86 3) false))

;; http://www.4clojure.com/problem/93
(def problem-93
  (letfn [(last-level? [coll]
            (every? (complement sequential?) coll))]
    (fn [coll]
      (if (every? last-level? coll)
        coll
        (recur
          (mapcat (fn [coll]
                    (if (last-level? coll)
                      [coll]
                      coll)) coll))))))

(is (= (problem-93 [["Do"] ["Nothing"]])
       [["Do"] ["Nothing"]]))
(is (= (problem-93 [[[[:a :b]]] [[:c :d]] [:e :f]])
       [[:a :b] [:c :d] [:e :f]]))
(is (= (problem-93 '((1 2)((3 4)((((5 6)))))))
       '((1 2)(3 4)(5 6))))

;; http://www.4clojure.com/problem/98
(def problem-98
  (fn [f D]
    (loop [d D
           result #{}]
      (if (seq d)
        (let [e (first d)
              r (f e)]
          (recur
            (rest d)
            (conj result (reduce (fn [out e]
                                   (if (= (f e) r)
                                     (conj out e)
                                     out)) #{e} D))))
        result))))

(is (= (problem-98 #(* % %) #{-2 -1 0 1 2})
       #{#{0} #{1 -1} #{2 -2}}))
(is (= (problem-98 #(rem % 3) #{0 1 2 3 4 5})
       #{#{0 3} #{1 4} #{2 5}}))
(is (= (problem-98 identity #{0 1 2 3 4})
       #{#{0} #{1} #{2} #{3} #{4}}))
(is (= (problem-98 (constantly true) #{0 1 2 3 4})
       #{#{0 1 2 3 4}}))

;; http://www.4clojure.com/problem/102
(def problem-102
  (fn [s]
    (let [[f* & s*] (clojure.string/split s #"-")]
      (->> (map (fn [[f & r]]
                  ;; use clojure.string/capitalize
                  (apply str (clojure.string/upper-case f) r)) s*)
           (apply str f*)))))

(is (= (problem-102 "something") "something"))
(is (= (problem-102 "multi-word-key") "multiWordKey"))
(is (= (problem-102 "leaveMeAlone") "leaveMeAlone"))

;; http://www.4clojure.com/problem/103
(def problem-103
  (fn k-combinations [k s]
    (if (= k 1)
      (into #{} (map (comp set list) s))
      (let [prev-combinations (k-combinations (dec k) s)]
        (->> (for [prev prev-combinations
                   el s]
               (conj prev el))
             (filter (fn [s] (= k (count s))))
             (into #{}))))))

(is (= (problem-103 1 #{4 5 6}) #{#{4} #{5} #{6}}))
(is (= (problem-103 10 #{4 5 6}) #{}))
(is (= (problem-103 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
(is (= (problem-103 3 #{0 1 2 3 4})
       #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
(is (= (problem-103 4 #{[1 2 3] :a "abc" "efg"})
       #{#{[1 2 3] :a "abc" "efg"}}))
(is (= (problem-103 2 #{[1 2 3] :a "abc" "efg"})
       #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
         #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))

;; http://www.4clojure.com/problem/105
(def problem-105
  (fn [coll]
    (loop [out {}
           coll coll
           last-keyword nil]
      (if (seq coll)
        (if (keyword (first coll))
          (recur (assoc out (first coll) [])
                 (rest coll)
                 (first coll))
          ;; should use update, but 4clojure uses old version of clojure
          (recur (assoc out last-keyword (conj (get out last-keyword)
                                               (first coll)))
                 (rest coll)
                 last-keyword))
        out))))

(is (= {} (problem-105 [])))
(is (= {:a [1]} (problem-105 [:a 1])))
(is (= {:a [1], :b [2]} (problem-105 [:a 1, :b 2])))
(is (= {:a [1 2 3], :b [], :c [4]} (problem-105 [:a 1 2 3 :b :c 4])))

;; http://www.4clojure.com/problem/108
(def problem-108
  (fn [& seqs]
    (letfn [(merge-seqs [seqs]
              (loop [seqs seqs
                     res []]
                (lazy-seq
                  (if (some seq seqs)
                    (let [m (reduce (fn [out s]
                                      (if-let [seqs-for-key (get out (first s))]
                                        (assoc out (first s) (conj seqs-for-key s))
                                        (assoc out (first s) [s])))
                                    {} (filter seq seqs))
                          min-key (apply min (keys m))
                          min-keys-seqs-count (count (filter seq (get m min-key)))
                          m (assoc m min-key (map rest (get m min-key)))]

                      (concat (repeat min-keys-seqs-count min-key) (merge-seqs (apply concat (vals m)))))))))]
      (let [merged (merge-seqs seqs)]
        (->> merged
             (partition-by identity)
             (some (fn [g] (when (= (count seqs) (count g))
                             (first g)))))))))

(is (= 3 (problem-108 [3 4 5])))
(is (= 4 (problem-108 [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
(is (= 7 (problem-108 (range) (range 0 100 7/6) [2 3 5 7 11 13])))
(is (= 64 (problem-108 (map #(* % % %) (range))
                       (filter #(zero? (bit-and % (dec %))) (range))
                       (iterate inc 20))))

;; http://www.4clojure.com/problem/110
(def problem-110
  (fn pronunciations-fn [coll]
    (lazy-seq
      (let [pronunciations
            (mapcat
              (fn [coll]
                [(count coll) (first coll)]) (partition-by identity coll))]
        (cons pronunciations (pronunciations-fn pronunciations))))))

(is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (problem-110 [1]))))
(is (= [3 1 2 4] (first (problem-110 [1 1 1 4 4]))))
(is (= [1 1 1 3 2 1 3 2 1 1] (nth (problem-110 [1]) 6)))
(is (= 338 (count (nth (problem-110 [3 2]) 15))))

;; http://www.4clojure.com/problem/114
(def problem-114
  (fn [n p coll]
    ;; it is better to decrease n till 0, instead of increasing new counter k
    (letfn [(gen [k [f & r]]
              (lazy-seq
                (let [k (if (p f) (inc k) k)]
                  (when (< k n)
                    (cons f (gen k r))))))]
      (gen 0 coll))))

(is (= [2 3 5 7 11 13]
       (problem-114 4 #(= 2 (mod % 3))
                    [2 3 5 7 11 13 17 19 23])))
(is (= ["this" "is" "a" "sentence"]
       (problem-114 3 #(some #{\i} %)
                    ["this" "is" "a" "sentence" "i" "wrote"])))
(is (= ["this" "is"]
       (problem-114 1 #{"a"}
                    ["this" "is" "a" "sentence" "i" "wrote"])))

;; http://www.4clojure.com/problem/115
(def problem-115
  (fn [n]
    (let [n (->> (str n)
                 (map #(Integer. (str %))))
          half (int (/ (count n) 2))]

      (= (apply + (take half n))
         (apply + (take-last half n))))))

(is (= true (problem-115 11)))
(is (= true (problem-115 121)))
(is (= false (problem-115 123)))
(is (= true (problem-115 0)))
(is (= false (problem-115 88099)))
(is (= true (problem-115 89098)))
(is (= true (problem-115 89089)))
(is (= (take 20 (filter problem-115 (range)))
       [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))

;; http://www.4clojure.com/problem/116
(def problem-116
  (fn [n]
    (letfn [(is-prime? [x]
              (if (>= x 2)
                (loop [i 2]
                  (if (<= (* i i) x)
                    (if (zero? (rem x i))
                      false
                      (recur (inc i)))
                    true))))
            (find-prime [x f]
              (when (> x 1)
                (if (is-prime? x)
                  x
                  (recur (f x) f))))]
      (if (is-prime? n)
        (let [next-prime (find-prime (inc n) inc)
              prev-prime (find-prime (dec n) dec)]
          (if (and next-prime prev-prime)
            (= (/ (+ prev-prime next-prime) 2) n)
            false))
        false))))

(is (= false (problem-116 4)))
(is (= true (problem-116 563)))
(is (= 1103 (nth (filter problem-116 (range)) 15)))

;; http://www.4clojure.com/problem/132
(def problem-132
  ;; this does not work because coll may be infinite lazy-seq
  ;; does not work for test with fib seq
  (fn [pred value coll]
    (if (seq coll)
      (loop [result [(first coll)]
             coll (rest coll)]
        (if (seq coll)
          (if (pred (last result) (first coll))
            (recur (conj result value (first coll))
                   (rest coll))
            (recur (conj result (first coll))
                   (rest coll)))
          result))
      [])))

(def problem-132*
  (fn problem-132* [pred value coll]
    (lazy-seq
      (if-let [f (first coll)]
        (if-let [s (second coll)]
          (if (pred f s)
            (cons f (cons value (problem-132* pred value (rest coll))))
            (cons f (problem-132* pred value (rest coll))))
          (cons f (problem-132* pred value (rest coll))))))))

(is (= '(1 :less 6 :less 7 4 3) (problem-132* < :less [1 6 7 4 3])))
(is (= '(2) (problem-132* > :more [2])))
(is (= [0 1 :x 2 :x 3 :x 4]
       (problem-132 #(and (pos? %) (< % %2)) :x (range 5))))
(is (empty? (problem-132* > :more ())))
(is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
       (take 12 (->> [0 1]
                     (iterate (fn [[a b]] [b (+' a b)]))
                     (map first)
                     (problem-132* (fn [a b]
                                     (= (mod a 2) (mod b 2)))
                                   :same)))))

;; http://www.4clojure.com/problem/137
(def problem-137
  (fn [n base]
    (vec (loop [n n
                result (list)]
           (if (< n base)
             (cons n result)
             (recur (quot n base) (cons (rem n base) result)))))))

(is (= [1 2 3 4 5 0 1] (problem-137 1234501 10)))
(is (= [0] (problem-137 0 11)))
(is (= [1 0 0 1] (problem-137 9 2)))
(is (= [1 0] (let [n (rand-int 100000)] (problem-137 n n))))
(is (= [16 18 5 24 15 1] (problem-137 Integer/MAX_VALUE 42)))

;; http://www.4clojure.com/problem/144
(def problem-144
  (fn [v & fns]
    (let [fns (cycle fns)]
      (letfn [(generate-seq [v fns]
                (lazy-seq
                  (let [result ((first fns) v)]
                    (cons result (generate-seq result (rest fns))))))]
        (cons v (generate-seq v fns))))))

(is (= (take 3 (problem-144 3.14 int double)) [3.14 3 3.0]))
(is (= (take 5 (problem-144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
(is (= (take 12 (problem-144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))

;; http://www.4clojure.com/problem/148
(def problem-148
  (fn [n a b]
    (letfn [(sum-progression [n]
              (if (> n 0)
                (/ (*' n (inc n)) 2)
                0))
            (count-divisors [n x]
              (if (zero? (rem n x))
                (dec (quot n x))
                (quot n x)))]
      (-' (+' (*' (sum-progression (count-divisors n a)) a)
              (*' (sum-progression (count-divisors n b)) b))
          (*' (sum-progression (count-divisors n (* a b))) (* a b))))))

(is (= 0 (problem-148 3 17 11)))
(is (= 23 (problem-148 10 3 5)))
(is (= 233168 (problem-148 1000 3 5)))
(is (= "2333333316666668" (str (problem-148 100000000 3 5))))
(is (= "110389610389889610389610"
       (str (problem-148 (* 10000 10000 10000) 7 11))))
(is (= "1277732511922987429116"
       (str (problem-148 (* 10000 10000 10000) 757 809))))
(is (= "4530161696788274281"
       (str (problem-148 (* 10000 10000 1000) 1597 3571))))

;; http://www.4clojure.com/problem/150
(def problem-150
  (letfn [(palindrome? [x]
            (= (str x) (apply str (reverse (str x)))))]
    (fn palindromes [n]
      (lazy-seq
        (if (palindrome? n)
          (cons n (palindromes (inc n)))
          (palindromes (inc n)))))))

(is (= (take 26 (problem-150 0))
       [0 1 2 3 4 5 6 7 8 9
        11 22 33 44 55 66 77 88 99
        101 111 121 131 141 151 161]))
(is (= (take 16 (problem-150 162))
       [171 181 191 202
        212 222 232 242
        252 262 272 282
        292 303 313 323]))
(is (= (take 6 (problem-150 1234550000))
       [1234554321 1234664321 1234774321
        1234884321 1234994321 1235005321]))
(is (= (first (problem-150 (* 111111111 111111111)))
       (* 111111111 111111111)))
(is (= (set (take 199 (problem-150 0)))
       (set (map #(first (problem-150 %)) (range 0 10000)))))
(is (= (nth (problem-150 0) 10101) 9102019))
(is (= true (apply < (take 6666 (problem-150 9999999)))))

;; http://www.4clojure.com/problem/158
(def problem-158
  (fn [f]
    (fn [& args]
      (loop [res (f (first args))
             args (rest args)]
        (if (fn? res)
          (recur (res (first args))
                 (rest args))
          res)))))

(= 25 ((problem-158 (fn [a]
                      (fn [b]
                        (* a b)))) 5 5))

;; http://www.4clojure.com/problem/171
(def problem-171
  (fn [coll]
    (->> coll
         (sort)
         (distinct)
         (reduce
           (fn [out el]
             (let [last-interval (last out)]
               (if (= (last last-interval) (dec el))
                 (conj (into [] (butlast out)) (assoc last-interval 1 el))
                 (conj out [el el])))) []))))

(is (= (problem-171 [1 2 3]) [[1 3]]))
(is (= (problem-171 [10 9 8 1 2 3]) [[1 3] [8 10]]))
(is (= (problem-171 [1 1 1 1 1 1 1]) [[1 1]]))
(is (= (problem-171 []) []))
(is (= (problem-171 [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
       [[1 4] [6 6] [9 11] [13 17] [19 19]]))

;; http://www.4clojure.com/problem/177
(def problem-177
  (fn [s]
    (let [pairs {\] \[
                \} \{
                \) \(}]
      (letfn [(open? [p]
                (contains? (into #{} (vals pairs)) p))
              (close? [p]
                (contains? (into #{} (keys pairs)) p))
              (pair? [p l]
                (= (get pairs p) l))]
        (loop [stack []
               [f & r :as s] s]
          (if (seq s)
            (cond
              (open? f) (recur (conj stack f) r)
              (close? f) (if (pair? f (last stack))
                           (recur (into [] (butlast stack)) r)
                           false)
              :else (recur stack r))
            ;; end of string
            (zero? (count stack))))))))

(is (problem-177 "This string has no brackets."))
(is (problem-177 "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))

;; http://www.4clojure.com/problem/195
(def problem-195
  (fn [n]
    (letfn [(generate-seq [opened closed current-seq result]
              (cond
                (and (> opened 0)
                     (= opened closed)) (generate-seq (dec opened)
                                                      closed
                                                      (conj current-seq "(")
                                                      result)
                (< opened closed) (do
                                    (when (> opened 0)
                                      (generate-seq (dec opened)
                                                    closed
                                                    (conj current-seq "(")
                                                    result))
                                    (when (> closed 0)
                                      (generate-seq opened
                                                    (dec closed)
                                                    (conj current-seq ")")
                                                    result)))

                (= opened closed 0) (swap! result conj (apply str (into [] (concat current-seq (repeat closed ")")))))

                :else (swap! result conj (apply str current-seq))))]
      (let [result (atom #{})]
        (if (zero? n)
          #{""}
          (do
            (generate-seq n n [] result)
            @result))))))

(is (= [#{""} #{"()"} #{"()()" "(())"}]
       (map (fn [n] (problem-195 n)) [0 1 2])))

(def problem-195*
  (fn problem-195*
    [n]
    (cond
      (= n 0) #{""}
      (= n 1) #{"()"}
      :else (let [prev-result (problem-195* (dec n))]
              (into #{} (mapcat
                          (fn [coll]
                            (apply conj [(str "()" coll)
                                         (str "(" coll ")")
                                         (str coll "()")]
                                   (loop [start (str (first coll))
                                          end (apply str (rest coll))
                                          result #{}]
                                     (if (seq end)
                                       (recur (str start (first end))
                                              (apply str (rest end))
                                              (conj result (str start "()" end)))
                                       result)))) prev-result))))))

(is (= [#{""} #{"()"} #{"()()" "(())"}]
       (map (fn [n] (problem-195* n)) [0 1 2])))
