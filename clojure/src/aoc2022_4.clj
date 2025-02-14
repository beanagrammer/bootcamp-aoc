(ns aoc2022-4
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(defn parse-assignment-pair
  "Parse an assignment pair into a range
   Input: pair=(input string)
   Output: range=(range of numbers)
   Example: '24-30' -> range(24 30) -> [24 25 26 27 28 29 30]
   "
  [pair]
  (->> (str/split pair #"-")
       (map #(parse-long %))
       ((fn [[x y]] (range x (+ y 1))))
       (apply vector)))

(defn is-fully-contained?
  "Check if one assignment is fully contained in the other
   Input: a=(assignment pair)
   Input: b=(assignment pair)
   Output: true/false
   Example: [1 2 3 4 5] [3 4 5] -> true | [3 4 5] [4 5 6 7] -> false 
   "
  [a b]
  (let [a-set (set a)
        b-set (set b)]
    (cond
      (every? b-set a-set) true
      (every? a-set b-set) true
      :else false)))

(defn is-overlapping?
  "Check if one assignment is overlapping with the other, any intersection between the two
   Input: a=(assignment pair)
   Input: b=(assignment pair)
   Output: true/false
   Example: [1 2 3 4 5] [3 4 5 6 7] -> true | [3 4 5] [4 5 6 7] -> true
   "
  [a b]
  (let [a-set (set a)
        b-set (set b)
        intersection (set/intersection a-set b-set)]
    (seq intersection)))

(defn input-assignments
  "Read the input file and parse the assignment pairs
   Input: None
   Output: assignment-pairs=(list of assignment pairs)
   Example: 2-4,6-8
            2-3,4-5
   -> [[[2 3 4] [6 7 8]] [[2 3] [4 5]]]
   "
  []
  (->> (slurp "resources/day4.2022.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map #(map parse-assignment-pair %))))

(defn fully-contained-assignments
  "Count the number of assignment pairs where one is fully contained in the other
   Input: assignment-pairs=(list of assignment pairs)
   Output: count=(number of assignment pairs where one is fully contained in the other)
   Example: [[[2 3 4] [6 7 8]] [[2 3] [4 5]]] -> 1
   "
  [assignment-pairs]
  (->> assignment-pairs
       (map #(is-fully-contained? (first %) (second %)))
       (filter true?)
       (count)))

(defn overlapping-assignments
  "Count the number of assignment pairs where one is overlapping with the other
   Input: assignment-pairs=(list of assignment pairs)
   Output: count=(number of assignment pairs where one is overlapping with the other)
   Example: [[[2 3 4] [6 7 8]] [[2 3] [4 5]]] -> 1
   "
  [assignment-pairs]
  (->> assignment-pairs
       (map #(is-overlapping? (first %) (second %)))
       (filter seq?)
       (count)))

(comment
  (def assignment-pairs (input-assignments))

  (println (is-overlapping? (parse-assignment-pair "24-66") (parse-assignment-pair "24-25")))

  (println (fully-contained-assignments assignment-pairs))
  (println (overlapping-assignments assignment-pairs)))