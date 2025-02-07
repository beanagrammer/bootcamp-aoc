(ns aoc2018-1
  (:require [clojure.string :as str])) ;; library for string manipulation

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...



;; "let" is a way to define a local variable
;; "step1" is a local variable that is assigned the result of the expression (str/split input #"\s+")
;; "parse-fn" is a local variable that is assigned the result of the expression #(Integer/parseInt %)
;; "step2" is a local variable that is assigned the result of the expression (map parse-fn step1)
;; "step2" is returned
;; so step2 calls already defined parse-fn on step1
(def numbers-step-by-step
  (let [input (slurp "resources/day1.sample.txt")
        step1 (str/split input #"\s+")
        parse-fn #(Integer/parseInt %)
        step2 (map parse-fn step1)]
    (vec step2)))  ;; Convert lazy sequence to vector


;; Parse input file into sequence of numbers
(def numbers
  (->> (slurp "resources/day1.sample.txt") ;; read the input file
       (str/split-lines) ;; split the input file into lines
       (map parse-long))) ;; parse the lines into integers

;; Part 1: Sum all numbers
(def part1
  (reduce + numbers)) ;; returns the sum of all numbers


;; Part 2: Find first repeated frequency
(defn part2 [numbers]
  (loop [seen #{0}           ;; Set of seen frequencies (immutable)
         sum 0               ;; Current running sum (immutable)
         nums (cycle numbers)] ;; Infinite sequence of numbers (Since its a frequency, it will repeat)
    (let [next-sum (+ sum (first nums))] ;; 
      (if (seen next-sum)    ;; Check if next-sum is in seen set
        next-sum ;; TRUE, early return the duplicate
        (recur (conj seen next-sum) ;; FALSE, calls recur with rest of the numbers
               next-sum
               (rest nums))))))


;; Part 2 using reduce
(defn part2-reduce [numbers]
  (let [freqs (reductions + 0 (cycle numbers))]  ;; Generate infinite sequence of running sums
    (->> freqs
         (reduce (fn [seen freq]                 ;; Accumulate seen frequencies acc = seen arg = freq
                   (if (seen freq)                ;; If we've seen this frequency
                     (reduced freq)               ;; Return it immediately
                     (conj seen freq)))           ;; Otherwise add to seen set
                 #{0}))))                         ;; Start with empty set

;; Print results
(comment
  (println "Part 1:" part1)
  (println "Part 2:" (part2 numbers))
  (println "Part 2 (reduce):" (part2-reduce numbers)))

