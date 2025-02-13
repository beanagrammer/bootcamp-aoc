(ns aoc2018-5-nested
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


(defn matching-case? [char1 char2]
  (= 32 (Math/abs (- (int char1) (int char2)))))


(defn check-polarity
  "Check if the left and right characters are of opposite polarity
   Input: left, centre, right i.e abc -> left a, centre C, right c
   Output: {:is-polarity true :side :left} or {:is-polarity true :side :right} or {:is-polarity false :side nil}"
  [left centre right]
  (if (matching-case? left centre)
    {:is-polarity true :side :left}
    (if (and (not= right \.) (matching-case? centre right))
      {:is-polarity true :side :right}
      {:is-polarity false :side nil})))

(defn remove-polarity
  [side mid input]
  (case side
    :left (str (subs input 0 (dec mid))
               (subs input (inc mid)))
    :right (str (subs input 0 mid)
                (subs input (+ mid 2)))
    input))

(defn safe-nth [s idx default]
  (if (and (>= idx 0) (< idx (count s)))
    (nth s idx)
    default))


"UNCLEANED VERSION"
(defn nested-process-polymer [input]
  (if (<= (count input) 1)
    input
    (let [mid (quot (count input) 2)

          polarity-result (check-polarity
                           (safe-nth input (dec mid) \.)
                           (safe-nth input mid \.)
                           (safe-nth input (inc mid) \.))]

      (if (:is-polarity polarity-result)

        (let [new-input (remove-polarity (:side polarity-result) mid input)]
          (recur new-input))


        (let [left-half (subs input 0 mid)
              right-half (subs input mid)

              processed-left (nested-process-polymer left-half)
              processed-right (nested-process-polymer right-half)]


          (if (and (seq processed-left)
                   (seq processed-right)
                   (matching-case? (last processed-left)
                                   (first processed-right)))

            (let [merged (str (subs processed-left 0
                                    (dec (count processed-left)))
                              (subs processed-right 1))]
              (recur merged))

            (str processed-left processed-right)))))))

(def polymer-string
  (-> "resources/day5.sample.txt"
      slurp          ; Read the entire file
      str/trim))
;
(println (count (nested-process-polymer polymer-string)))
;(println (process-polymer "dabAcCaCBAcCcaDA"))
"
if input is <= 1 return input
else 
 if check-polarity
   return middle-check(remove-polarity)
 else 
 l = middle-check(subsring(0, mid))
 r = middle-check(subsring(mid+1, end))
 return concat(l, r)
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
