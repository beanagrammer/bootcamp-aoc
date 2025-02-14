(ns aoc2020-4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;byr (Birth Year)
;iyr (Issue Year)
;eyr (Expiration Year)
;hgt (Height)
;hcl (Hair Color)
;ecl (Eye Color)
;pid (Passport ID)
;cid (Country ID)

;PART 1 Passport Spec
(s/def ::byr (s/and string?  #(not (str/blank? %))))
(s/def ::iyr (s/and string? #(not (str/blank? %))))
(s/def ::eyr (s/and string? #(not (str/blank? %))))
(s/def ::hgt (s/and string? #(not (str/blank? %))))
(s/def ::hcl (s/and string? #(not (str/blank? %))))
(s/def ::ecl (s/and string? #(not (str/blank? %))))
(s/def ::pid (s/and string? #(not (str/blank? %))))
(s/def ::cid (s/and string? #(not (str/blank? %))))
(s/def ::sample (s/keys :req-un [::ecl ::pid]))
(s/def ::simple-passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                                 :opt-un [::cid]))
;PART 2 Passport Spec
;YEAR RANGES
(s/def ::four-digit-number (s/and string? #(re-matches #"\d{4}" %)))
(s/def ::byr-range #(when % (<= 1920 (parse-long %) 2002)))
(s/def ::iyr-range #(when % (<= 2010 (parse-long %) 2021)))
(s/def ::eyr-range #(when % (<= 2020 (parse-long %) 2031)))

(s/def ::byr-strict (s/and ::four-digit-number ::byr-range))
(s/def ::iyr-strict (s/and ::four-digit-number ::iyr-range))
(s/def ::eyr-strict (s/and ::four-digit-number ::eyr-range))

;HEIGHT RANGES
(s/def ::inch-height (s/and string?
                            #(re-matches #"\d{2}in" %)
                            #(<= 59 (parse-long (subs % 0 2)) 76)))
(s/def ::cm-height (s/and string?
                          #(re-matches #"\d{3}cm" %)
                          #(<= 150 (parse-long (subs % 0 3)) 193)))
(s/def ::hgt-strict (s/or :inch ::inch-height :cm ::cm-height))

(s/def ::hcl-strict (s/and string? #(re-matches #"^#[0-9a-f]{6}$" %)))
(s/def ::ecl-strict #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid-strict (s/and string? #(re-matches #"^\d{9}$" %)))

(defn valid-field? [passport field spec]
  (when-let [value (get passport field)]
    (s/valid? spec value)))

(s/def ::strict-passport
  (s/and ::simple-passport
         #(valid-field? % :byr ::byr-strict)
         #(valid-field? % :iyr ::iyr-strict)
         #(valid-field? % :eyr ::eyr-strict)
         #(valid-field? % :hgt ::hgt-strict)
         #(valid-field? % :hcl ::hcl-strict)
         #(valid-field? % :ecl ::ecl-strict)
         #(valid-field? % :pid ::pid-strict)))

(defn get-passports
  "Parse passport data from file
   Input: filename=(input file path)
   Output: passports=(list of passport maps)
   Example: 'ecl:gry pid:860033327\neyr:2020 hcl:#fffffd\n\niyr:2013 ecl:amb'
   -> [[['ecl:gry' 'pid:860033327'] ['eyr:2020' 'hcl:#fffffd']] [['iyr:2013' 'ecl:amb']]]"
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (partition-by empty?)
       (remove #(= % '(""))) ; remove empty lines
       (map #(str/join " " %))  ; Join lines within each group with spaces
       (map #(str/split % #" ")))) ; Split each group by spaces


(defn is-valid-simple-passport? [passport]
  (s/valid? ::simple-passport passport))

(defn is-valid-passport? [passport]
  (s/valid? ::strict-passport passport))

(defn parse-single-passport
  "Parse a single passport's data into a map
   Input: passport-fields=['ecl:gry' 'pid:860033327' ...]
   Output: {:ecl 'gry' :pid '860033327' ...}"
  [passport-fields]
  (->> passport-fields
       (map #(str/split % #":"))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))


(defn parse-passports
  "Parse all passports with index
   Input: passports-data=(list of passport field lists)
   Output: [[{:ecl 'gry' ...} {:ecl 'amb' ...}] ...]"
  [passports-data]
  (->> passports-data
       (map parse-single-passport)
       vec))


(defn count-valid-passports
  "Count the number of valid passports
   Input: method=(:simple :strict)
          passports=(list of passports)
   Output: count=(number of valid passports)
   Example: [[{:ecl 'gry' ...} {:ecl 'amb' ...}] ...] -> 2
   "
  [method passports]
  (->> passports
       (map #(cond
               (= method :simple) (is-valid-simple-passport? %)
               (= method :strict) (is-valid-passport? %)))
       (filter true?)
       count))

(comment
  (s/valid? ::sample
            {:ecl "gry"
             :pid "860033327"})
  (def raw-passports (get-passports "resources/day4.2020.txt"))
  (parse-passports raw-passports)
  (s/valid? ::inch-height "169in")
  (s/valid? ::cm-height "169cm")
  (s/valid? ::hgt-strict "169in")
  (valid-field? {:hcl "#7d3b0c"
                 :pid "431742871"
                 :ecl "hzl"
                 :hgt "169cm"
                 :cid "340"
                 :eyr "2023"
                 :iyr "2017"
                 :byr "1994"} :hgt ::hgt-strict)
  (is-valid-passport? {:hcl "#7d3b0c"
                       :pid "431742871"
                       :ecl "hzl"
                       :hgt "169cm"
                       :cid "340"
                       :eyr "2023"
                       :iyr "2017"
                       :byr "1994"})
  (count-valid-passports :simple (parse-passports raw-passports))
  (count-valid-passports :strict (parse-passports raw-passports)))