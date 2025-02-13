(ns aoc2018-4
  (:require [clojure.string :as str]))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 "주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라"
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, "11분"이 가장 빈번하게 잠들어 있던 '분'. 그럼 답은 20 * 11 = 220.


(defn parse-event
  "Parse the event part of the log entry
   Input: event-str
   Output: {:event :begin-shift :guard-id 10} or {:event :falls-asleep} or {:event :wakes-up}"
  [event-str]
  (cond
    (str/includes? event-str "Guard")
    {:event :begin-shift
     :guard-id (parse-long (re-find #"\d+" event-str))}

    (str/includes? event-str "falls asleep")
    {:event :falls-asleep}

    (str/includes? event-str "wakes up")
    {:event :wakes-up}))


(defn parse-time-values
  "Convert sequence of time strings into numbers
   Input: time-vals
   Output: {:year :month :day :hour :minute}"
  [time-vals]
  (->> time-vals
       (map parse-long)
       (zipmap [:year :month :day :hour :minute])))

(defn split-time-and-event
  "Split regex matches into time values and event string
   Input: matches
   Output: [time-vals event-str]"
  [matches]
  [(butlast matches) (last matches)])

(defn combine-time-and-event
  "Combine time map and parsed event into final result
   Input: [time-vals event-str]
   Output: {:year :month :day :hour :minute :event :guard-id}"
  [[time-vals event-str]]
  (merge (parse-time-values time-vals)
         (parse-event event-str)))

(defn parse-line
  "Parse a single log line into a map containing timestamp and event information
   Input: line i.e. [1518-11-01 00:00] Guard #10 begins shift
   Output: {:year :month :day :hour :minute :event :guard-id}"
  [line]
  (->> line
       (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)")
       rest
       split-time-and-event
       combine-time-and-event))

(defn initialize-guard
  "Initialize a guard's pattern map
   Input: patterns, guard-id
   Output: {:guard-id guard-id :patterns []} or {:guard-id guard-id :patterns patterns}"
  [patterns guard-id]
  (update patterns
          guard-id
          #(or % []))) ; % = patterns[guard-id]

(defn process-begin-shift
  "Process the begin shift event
   Input: guard-patterns, event
   Output: {:guard-id guard-id :patterns {:guard-id guard-id []}}"
  [guard-patterns event]
  (let [guard-id (:guard-id event)
        result {:guard-id guard-id
                :patterns (initialize-guard (:patterns guard-patterns) guard-id)}]
    (println "process-begin-shift" result)
    result))

(defn log-sleep-start
  "Log the start of a sleep period for the current guard
   Input: patterns, guard-id, minute
   Output: {:guard-id guard-id :patterns {:guard-id guard-id [{:start minute}]}}"
  [patterns guard-id minute]
  (update-in patterns
             [guard-id]
             #(conj % {:start minute})))

(defn log-sleep-end
  "Log the end of a sleep period for the current guard
   Gets the last pattern of guard-id and add end-minute to it. i.e {10 [{:start 45}]} -> {10 [45 50]}
   Input: patterns, guard-id, minute
   Output: {:guard-id guard-id :patterns {:guard-id guard-id [{:start minute} {:end minute}]}}"
  [patterns guard-id end-minute]
  (let [guard-patterns (get patterns guard-id)
        last-pattern (last guard-patterns)
        complete-pattern [(:start last-pattern) end-minute]
        updated-patterns (conj (vec (butlast guard-patterns)) complete-pattern)]
    (assoc patterns guard-id updated-patterns)))


(defn find-guard-patterns
  "Find the guard patterns for the given event
   Input: guard-patterns, event
   Output: {:guard-id guard-id :patterns {:guard-id guard-id [minute]}}"
  [guard-patterns event]
  (case (:event event)                   ; Then process
    :begin-shift (process-begin-shift guard-patterns event)
    :falls-asleep
    (update guard-patterns :patterns
            #(log-sleep-start % (:guard-id guard-patterns) (:minute event)))
    :wakes-up
    (update guard-patterns :patterns
            #(log-sleep-end % (:guard-id guard-patterns) (:minute event)))
    guard-patterns))

(defn parse-input-events
  "Parse input string into sequence of event maps with timestamp and event info
   Input: input-file
   Output: sequcne of {:year :month :day :hour :minute :event :guard-id}"
  [input-file]
  (->>
   (slurp input-file)
   (str/split-lines)
   (map parse-line)
   (sort-by (juxt :year :month :day :hour :minute))))


(defn calculate-guard-sleep-time
  "Calculate total sleep time for a guard's patterns
   Input: patterns i.e [[45 50] [55 60] [65 70]]
   Output: total sleep time i.e. 15"
  [patterns]
  (->> patterns
       (map #(- (second %) (first %)))  ; Calculate duration of each sleep period
       (reduce + 0)))


(defn find-sleepiest-guard
  "Find guard with maximum total sleep time
   Input: guard-patterns i.e {10 [[45 50] [55 60] [65 70]], 99 [[40 50]]}
   Output: [10 [[45 50] [55 60] [65 70]]]"
  [guard-patterns]
  (->> guard-patterns
       (map (fn [[guard-id patterns]]
              {:guard-id guard-id
               :total-sleep (calculate-guard-sleep-time patterns)
               :patterns patterns}))
       (sort-by :total-sleep >)
       first
       (#(vector (:guard-id %) (:patterns %)))))


(defn find-sleepiest-minute
  "Find the minute that the guard is asleep the most
   Input: guard-patterns i.e {10 [[45 50] [55 60] [65 70]]}
   Output: {:guard-id 10 :minute 0 :frequency 0}"
  [guard-patterns]
  (if (empty? (second guard-patterns))
    {:guard-id (first guard-patterns) :minute 0 :frequency 0}
    (let [guard-id (first guard-patterns)
          [minute freq] (->> (second guard-patterns)
                             (mapcat #(range (first %) (second %)))
                             frequencies
                             (sort-by val >)
                             first)]
      {:guard-id guard-id
       :minute minute
       :frequency freq})))


(defn generate-guard-patterns
  "Generate guard patterns from input events
   Input: input-events i.e [{:year 1518, :month 11, :day 1, :hour 0, :minute 0, :event :begin-shift, :guard-id 10} ...]
   Output: i.e {:guard-id 10 :patterns {10 [[45 50] [55 60] [65 70]], 99 [[40 50]]}}"
  [input-events]
  (reduce find-guard-patterns {:guard-id nil :patterns {}} input-events))


(def guard-patterns
  (->> (parse-input-events "resources/day4.sample2.txt")
       (generate-guard-patterns)))

(defn part1
  "Part 1: Find the guard that has the most minutes asleep and the minute they are asleep the most
   Logic Steps:
   1. Find the guard with the most total sleep time
   2. Find the minute that the guard is asleep the most
   3. Multiply the guard-id and the minute to get the result
   Input: guard-patterns i.e {:guard-id 10 :patterns {10 [[45 50] [55 60] [65 70]], 99 [[40 50]]}}
   Output: guard-id * minute"
  [guard-patterns]
  (->> (:patterns guard-patterns)
       find-sleepiest-guard
       find-sleepiest-minute
       (#(* (:guard-id %) (:minute %)))))

(defn part2
  "Part 2: Find the guard that has the most minutes asleep on the same minute and the minute they are asleep the most
   Logic Steps:
   1. Get all the guards' sleepiest minute [{:guard-id 10 :minute 0 :frequency 0} ...]
   2. Sort guards' sleepiest minute by frequency in descending order
   3. Get the first one and multiply the guard-id and the minute to get the result
   Input: guard-patterns i.e {:guard-id 10 :patterns {10 [[45 50] [55 60] [65 70]], 99 [[40 50]]}}
   Output: guard-id * minute"
  [guard-patterns]
  (->> (:patterns guard-patterns)
       (map find-sleepiest-minute)
       (sort-by :frequency >)
       first
       (#(* (:guard-id %) (:minute %)))))


;; Testing Blocks
(comment
  (def guard-patterns-test
    (->> (parse-input-events "resources/day4.sample2.txt")
         (generate-guard-patterns)))

  (println guard-patterns-test)

  (def sleepiest-guard (find-sleepiest-guard (:patterns guard-patterns-test)))
  (def sleepiest-minute (find-sleepiest-minute sleepiest-guard))

  (println "Sleepiest guard: " sleepiest-guard)
  (println "Sleepiest minute: " sleepiest-minute)
  (println "Part 1: " (* (:guard-id sleepiest-guard) (:minute sleepiest-minute)))

  (def guards-sleepiest-minutes (map find-sleepiest-minute (:patterns guard-patterns-test)))
  (def most-routine-guard (first (sort-by :frequency > guards-sleepiest-minutes)))

  (println "Guards sleepiest minutes: " guards-sleepiest-minutes)
  (println "Most routine guard: " most-routine-guard)

  (println "Part 2: " (* (:guard-id most-routine-guard) (:minute most-routine-guard))))

;; Run entire Solution
(comment
  (println (part1 guard-patterns))
  (println (part2 guard-patterns)))
