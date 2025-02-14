(ns aoc2018-5-stack
  (:require [clojure.string :as str]))

(defn matching-case?
  "Check if two characters are of opposite polarity
   Input: char1=(character 1) char2=(character 2)
   Output: true if the characters are of opposite polarity, false otherwise
   Example: char1='a' char2='B' -> false | char1='A' char2='a' -> true"
  [char1 char2]
  (= 32 (abs (- (int char1) (int char2)))))

(defn process-char
  "Process a single character against the polymer stack
   Input: stack=(current stack) current=(character to process)
   Output: updated stack
   Example: stack=['d' 'A'] current=\\a -> returned stack=['d']"
  [stack current]
  (let [peeked (peek stack)
        is-matching (and peeked (matching-case? peeked current))]
    (cond
      is-matching
      (pop stack)

      :else
      (conj stack current))))

(defn stack-process-polymer
  "Process a polymer string to remove all reactions using a stack
   Input: input=(input string)
   Output: processed string
   Example: 
   seq => [baAbdD]
   reduce=>
   [] b -> [b]
   [b] a -> [ba]
   [ba] A -> [b]
   [b] b -> [bb]
   [bb] d -> [bbd]
   [bbd] D -> [bb]
   apply str => bb
   "
  [input]
  (->> input
       (seq)
       (reduce process-char [])
       (apply str)))

(def polymer-string
  (-> "resources/day5.sample.txt"
      slurp
      str/trim))

;(println polymer)


; PART 2

(defn remove-polymer-pair
  "Remove a polymer pair from the polymer string
   Input: pair=(polymer pair) polymer-string=(polymer string)
   Output: polymer string without the pair
   Example: pair='a' polymer-string='dabAcCaCBAcCcaDA' -> 'dbcCCBcCcD'"
  [pair polymer-string]
  (-> polymer-string
      (str/replace (re-pattern (str "(?i)" pair)) "")))


(defn full-reaction-removed
  "Remove a polymer pair from the polymer string and process the polymer
   Input: removed-unit=(polymer pair) polymer-string=(polymer string)
   Output: polymer string without the pair and its length
   Example: removed-unit='a' polymer-string='dabAcCaCBAcCcaDA' -> 'dbcCCBcCcD' -> 'dbCBcD' -> 6"
  [removed-unit polymer-string]
  (->> polymer-string
       (remove-polymer-pair removed-unit)
       stack-process-polymer
       count))

(defn find-min-polyer
  [polymer-string]
  (->>
   (range (int \a) (inc (int \z)))
   (map #(full-reaction-removed (char %) polymer-string))
   (sort)
   (first)))

(comment
  (println "Part 1:" (count (stack-process-polymer polymer-string)))
  (println "Part 2:" (find-min-polyer polymer-string)))
