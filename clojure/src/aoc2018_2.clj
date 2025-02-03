(ns aoc2018-2
  (:require [clojure.string :as str])) ;; library for string manipulation
;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################

;; PART 1
(def input
  (->> (slurp "resources/day2.sample.txt")
       (str/split-lines))) ;; 
;; return number of dup and triples

(def check_dup_and_triples
  "Checks a string for characters that appear exactly twice and three times.
     Input: addr - string to check
     Returns: a map of character frequencies"
  (fn [addr]
    (let [char-freq (frequencies addr)        ;; Get frequency map of characters
          has-dup? (if (some #(= 2 %) (vals char-freq)) 1 0)     ;; Check if any char appears exactly twice
          has-triple? (if (some #(= 3 %) (vals char-freq)) 1 0)] ;; Check if any char appears exactly thrice
      [has-dup? has-triple?])))              ;; Return vector of [dup-count triple-count]

;; doseq = iterate over a sequece while printing each element (like foreach)
(defn part1
  "Solves part 1 of the puzzle. Reads input file and calculates checksum of all box IDs.
   Input: input - list of strings
   Returns: checksum of all box IDs"
  [input]
  (let [checksum-pairs (map check_dup_and_triples input)
        _ (println checksum-pairs)
        _ (println "\nStep 1 - Checksum pairs for each string:")
        _ (doseq [[string pair] (map vector input checksum-pairs)]
            (println string "=>" pair))

        doubles-count (->> checksum-pairs
                           (map first)
                           (reduce +))
        _ (println "\nStep 2 - Count of strings with doubles:" doubles-count)

        triples-count (->> checksum-pairs
                           (map second)
                           (reduce +))
        _ (println "Step 3 - Count of strings with triples:" triples-count)

        result (* doubles-count triples-count)]
    (println "\nStep 4 - Final checksum (doubles * triples):" result)
    result))

;; PART 2
;; sort input by
(def sorted-input (sort input))
(println sorted-input)
(for [i sorted-input]

  (println i))

;; Test cases
(println "\n=== Test Cases ===")
(println "abbcde =>" (check_dup_and_triples "abbcde"))  ;; Should return [1 0]
(println "bababc =>" (check_dup_and_triples "bababc"))  ;; Should return [1 1]

;; Calculate result
(println "\n=== Part 1 Solution ===")
(println "Final result:" (part1 input))

;; Helper function to count character differences between two strings
(defn count-differences
  "Counts the number of characters that are different between two strings.
   Input: str1, str2 - strings to compare
   Returns: count of different characters"
  [str1 str2]
  (count (filter false? (map = str1 str2))))

;; Find pairs with exactly one difference and return common letters
(defn find-one_diff_pair
  "Finds pairs with exactly one difference between two strings.
   Input: addresses - list of strings
   Returns: list of maps with :pair and :common"
  [addresses]
  (println "\nChecking all address pairs:")
  (for [addr1 addresses ;; outer loop
        addr2 addresses ;; inner loop
        :when (and (not= addr1 addr2) ;; check if addresses are not the same
                   (let [diff-count (count-differences addr1 addr2)]
                     (println "\nComparing:" addr1 "vs" addr2)
                     (println "Difference count:" diff-count)
                     (= 1 diff-count)))]
    (let [common-chars (map #(if (= %1 %2) %1 nil) addr1 addr2) ;; returns common characters
          result {:pair [addr1 addr2]
                  :common (apply str (remove nil? common-chars))}] ;; removes nil values
      (println "Found match!")
      (println "Common characters:" (remove nil? common-chars))
      (println "Result:" result)
      result)))

;; Test and run part 2
(defn part2 [input]
  (let [result (find-one_diff_pair input)]
    (println "\nStep 1 - Found pairs with one difference:")
    (doseq [r result]
      (println "Pair:" (:pair r))
      (println "Common letters:" (:common r)))
    (-> result first :common)))

;; Run part 2
(println "\n=== Part 2 Solution ===")
(println "Common letters:" (part2 sorted-input))