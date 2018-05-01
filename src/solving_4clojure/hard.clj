(ns solving-4clojure.hard
  (:require [clojure.test :refer [is]]))

;; http://www.4clojure.com/problem/53
(def problem-53
  (fn [coll]
    (loop [longest-increasing-seq []
           current-increasing-seq [(first coll)]
           [cur & rest :as coll] (rest coll)]
      (if (seq coll)
        (if (> cur (last current-increasing-seq))
          (recur longest-increasing-seq
                 (conj current-increasing-seq cur)
                 rest)
          (let [longest-increasing-seq
                (if (> (count current-increasing-seq)
                       1
                       (count longest-increasing-seq))
                  current-increasing-seq
                  longest-increasing-seq)]
            (recur longest-increasing-seq
                   [cur]
                   rest)))
        (if (> (count current-increasing-seq)
               (count longest-increasing-seq)
               1)
          current-increasing-seq
          longest-increasing-seq)))))

(is (= (problem-53 [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(is (= (problem-53 [5 6 1 3 2 7]) [5 6]))
(is (= (problem-53 [2 3 3 4 5]) [3 4 5]))
(is (= (problem-53 [7 6 5 4]) []))
