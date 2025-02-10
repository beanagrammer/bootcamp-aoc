(ns aoc2018-3
  (:require [clojure.string :as str])) ;; library for string manipulation


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def input
  "Reads the input file and returns a list of claims.
   Each claim is a map with the following keys:
   - id: the ID of the claim
   - left: the left coordinate of the claim
   - top: the top coordinate of the claim
   - width: the width of the claim
   - height: the height of the claim
   - area: the area of the claim"
  (->> (slurp "resources/day3.sample.txt")
       (str/split-lines)
       (map #(re-seq #"\d+" %)) ; get all numbers in the line
       (map (fn [nums]
              (let [[id left top width height] (map #(parse-long %) nums)]
                {:id id
                 :left left
                 :top top
                 :width width
                 :height height
                 :area (* width height)})))
       (vec)))

(comment
  (println input))

;; Create grid and process claims
(def grid-size 1000)  ; based on your example output

(defn print-grid
  [grid]
  (doseq [row grid]
    (println row)))

(defn fill-width
  "Fills a width line with the given ID and left coordinate.
   Input: id - the ID of the claim
          left - the left coordinate of the claim
          width - the width of the claim
   Returns: a list of strings representing the width line"
  [id left width]
  (let [temp-line (vec (repeat grid-size "."))]
    (reduce (fn [row idx]
              (assoc row idx (str id)))
            temp-line
            (range left (+ left width))))) ; range creates a sequence of numbers from left to left + width

(defn fill-height
  "Fills a height line with the given ID and top coordinate.
   Input: grid-to-fill - the grid to fill
          id - the ID of the claim
          left - the left coordinate of the claim
          top - the top coordinate of the claim
          width - the width of the claim
          height - the height of the claim
   Returns: a list of strings representing the height line"
  [grid-to-fill id left top width height]
  (let [width-line (fill-width id left width)]
    (map-indexed (fn [idx row]
                   (if (and (>= idx top) (< idx (+ top height)))
                     (map-indexed (fn [col val]
                                    ; check if current grid[idx][col] is not "." and width-line[col] is not "."
                                    (if (and (not= val ".") (not= (nth width-line col) "."))
                                      "X"  ;; Collision - both have values
                                      (if (not= (nth width-line col) ".")
                                        (nth width-line col)  ;; Use width-line value
                                        val)))  ;; Keep original value (".")
                                  row)
                     row))
                 grid-to-fill)))



(def grid
  (vec (repeat grid-size (vec (repeat grid-size ".")))))

(defn process-input
  "Processes the input and fills the grid with the claims.
   Input: input - the list of claims
   Returns: the final grid with the claims filled in"
  [input]
  (reduce (fn [current-grid {:keys [id left top width height]}]
            (println "Processing claim:" id)
            (vec (doall (fill-height current-grid id left top width height))))
          grid
          input))

;; Create and print final grid
(def final-grid (process-input input))

;; PART 1
;; Count overlapping claims (X's)
(def overlap-count
  (->> final-grid
       flatten
       (filter #(= "X" %))
       count))
;; Test the function
(comment
  (println "\nWidth line:")
  (println (fill-width 1 1 3))
  (println "\nModified grid:")
  (println "\nNumber of overlapping squares:" overlap-count))

;; PART 2
;; Count occurrences of each ID in the grid
(defn count-id-occurrences
  "Counts the occurrences of each ID in the grid.
   Input: grid - the final grid with the claims filled in
          input - the list of claims
   Returns: a list of maps with the following keys:
            - id: the ID of the claim
            - actual-count: the number of times the ID appears in the grid"
  [grid input]
  (->> grid
       flatten ;; flatten the grid to single sequence 
       (filter #(not (or (= "." %) (= "X" %))))  ;; Filter out "." and "X"
       (group-by identity)  ;; Group by ID {"id" ["id" "id" "id"]}
       (map (fn [[id occurrences]] ;; 
              {:id (parse-long id)
               :actual-count (count occurrences)}))
       ; #(for ...) creates a function that takes the previsous result and input
       ; and returns the claim that has the same id and area as the count-result
       (#(for [count-result %
               claim input
               :when (and (= (:id count-result) (:id claim))
                          (= (:actual-count count-result) (:area claim)))]
           claim))))

(comment
  (println (count-id-occurrences final-grid input)))
