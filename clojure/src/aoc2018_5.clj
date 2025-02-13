(ns aoc2018-5
  (:require [clojure.string :as str]))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.


(defn matching-case?
  "Check if two characters are of opposite polarity
   Input: char1=(character 1) char2=(character 2)
   Output: true if the characters are of opposite polarity, false otherwise
   Example: char1='a' char2='B' -> false | char1='A' char2='a' -> true"
  [char1 char2]
  (= 32 (Math/abs (- (int char1) (int char2)))))


(defn check-polarity
  "Check if the left and right characters are of opposite polarity
   Input: left=(left character) centre=(centre character) right=(right character)
   Output: 
       {:is-polarity true :side :left} or 
       {:is-polarity true :side :right} or 
       {:is-polarity false :side nil}"
  [left centre right]
  (if (matching-case? left centre)
    {:is-polarity true :side :left}
    (if (and (not= right \.) (matching-case? centre right))
      {:is-polarity true :side :right}
      {:is-polarity false :side nil})))

(defn remove-polarity
  "Remove the polarity at the given side and index
   Input: side=(side to remove) mid=(middle index) input=(input string)
   Output: new input with the polarity removed
   Example: input='cCaB' side=:left mid=1 -> 'aB'"
  [side mid input]
  (case side
    :left (str (subs input 0 (dec mid))
               (subs input (inc mid)))
    :right (str (subs input 0 mid)
                (subs input (+ mid 2)))
    input))

(defn safe-nth
  "Safely get index value
   Input: s=input idx=index default=default value if index is out of bounds
   Output: value at index or default value
   Example: s=ab idsx=2 default=. -> ."
  [s idx default]
  (if (and (>= idx 0) (< idx (count s)))
    (nth s idx)
    default))

(defn find-middle-reaction
  "Find the middle reaction
   Input: input=(input string) mid=(middle index)
   Output: {:is-polarity true :side :left} or {:is-polarity true :side :right} or {:is-polarity false :side nil}
   Example: input='cCa' mid=1 -> {:is-polarity true :side :left}"
  [input mid]
  (check-polarity (safe-nth input (dec mid) \.)
                  (safe-nth input mid \.)
                  (safe-nth input (inc mid) \.)))

(defn find-joining-reaction
  "Find the joining reaction
     Input: left=(left string) right=(right string)
     Output: true if there is a reaction at the joining point, false otherwise
     Example: left='a' right='B' -> false"
  [left right]
  (and (seq left)
       (seq right)
       (matching-case? (last left) (first right))))

(defn merge-segments
  "Merge the two segments
   Input: left=(left string) right=(right string)
   Output: merged string
   Example: left='a' right='B' -> 'aB'"
  [left right]
  (if (find-joining-reaction left right)
    (str (subs left 0 (dec (count left)))
         (subs right 1))
    (str left right)))

(defn process-polymer
  "Recursively processes a polymer string to remove all reactions
   Input: input=(input string)
   Output: processed string
   Example: input='cCaB' -> 'aB'"
  [input]
  (if (<= (count input) 1)
    input
    (let [mid (quot (count input) 2)
          reaction (find-middle-reaction input mid)]
      (cond
        (:is-polarity reaction)
        (recur (remove-polarity (:side reaction) mid input))

        :else
        (let [left-polymer (subs input 0 mid)
              right-polymer (subs input mid)
              reduced-left (process-polymer left-polymer)
              reduced-right (process-polymer right-polymer)
              merged-polymer (merge-segments reduced-left reduced-right)]
          (if (= merged-polymer input)
            merged-polymer
            (recur merged-polymer)))))))


(def polymer-string
  (-> "resources/day5.sample.txt"
      slurp          ; Read the entire file
      str/trim))
;
;(println (process-polymer "dabAcCaCBAcCcaDA"))
(println (count (process-polymer polymer-string)))
;(println (process-polymer "dabAcCaCBAcCcaDA"))

"
if input is <= 1 return input
else 
 if check-polarity
   return middle-check(remove-polarity)
 else 
 l = middle-check(subsring(0, mid))
 r = middle-check(subsring(mid+1, end))
 check polarity of l and r 
 if true
   return concat(l, r)
 else
   return concat(remove-polarity)(l, r))
"
"
dabAcCaCBAcCcaDA
l=dabAcCaCB 
remove-polarity=dabAaCB
remove-polarity=daBCB
l=daB
l=da
l=d
r=a
l+r=da
r=B
l+r=daB
r=CB
l+r=daBCB
r=C
l+r=daBCB
r=B
l+r=daBCB
r=CB
l+r=daBCB
prev=daBCB
prev=daBCB
r=AcCcaDA
remove-polarity=AcaDA
l=Aca
l=Ac
l=A
r=c
l+r=Ac
r=a
l+r=Aca
r=DA
l=D
r=A
l+r=DA
l+r=AcaDA
prev=AcaDA
l+r=daBCBAcaDA
"
