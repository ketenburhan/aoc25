(ns aoc25.day05
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(def test-data "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32")
(def data (slurp "resources/aoc25/day05.txt"))

; FACT: ranges have same number of digit at both sides.

(defn get-same-part-len
  [len left right]
  (first (filter (fn [n] (= (subs left 0 n) (subs right 0 n))) (reverse (range (inc len))))))


(defn create-range
  [range-line]
  (let [[low high] (str/split range-line #"-")
        len (count low)
        same-part-len (get-same-part-len len low high)
        same-part (subs low 0 same-part-len)]
    {:low           low
     :high          high
     :length        len
     :same-part-len same-part-len
     :same-part     same-part}))

(defn <=str
  [^Number len ^String left ^String right]
  (if-let [same-len (get-same-part-len len left right)]
    (if (= same-len len)
      true                                                  ; all same
      (< (int (get left same-len))
         (int (get right same-len))))                       ; different part comparison
    (< (int (first left)) (int (first right)))))            ; all different


(defn is-fresh?
  [grouped-ranges id]
  (not-empty
    (filter
      (fn [range]
        (if (str/starts-with? id (:same-part range))
          (and (<=str (count id) (:low range) id)
               (<=str (count id) id (:high range)))))
      (get grouped-ranges (count id)))))

(defn part1
  [input]
  (let [[ranges-str ids-str] (str/split input #"\n\n")
        range-lines (str/split-lines ranges-str)
        ranges (map create-range range-lines)
        grouped-by-len (group-by :length ranges)
        ids (str/split-lines ids-str)]
    (count (filter #(is-fresh? grouped-by-len %) ids))))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn overlaps-with?
  [range1 range2]
  (if (= range1 range2)
    true
    (let [len (:length range1)]
      (and (= len (:length range2))
           (<=str len (:low range1) (:high range2))
           (<=str len (:low range2) (:high range1))))))


(defn grow [component remaining]
  (let [[touching others]
        (reduce (fn [[ts os] s]
                  (if (seq (set/intersection component s))
                    [(conj ts s) os]
                    [ts (conj os s)]))
                [[] []]
                remaining)
        new-comp (reduce set/union component touching)]
    (if (empty? touching)
      [component others]
      (recur new-comp others))))
(defn merge-connected [indexes]
  (loop [sets (map set indexes)
         acc []]
    (if (empty? sets)
      (mapv vec acc)
      (let [[comp remaining] (grow (first sets) (rest sets))]
        (recur remaining (conj acc comp))))))


(defn union-ranges
  [ranges range-indices]
  (let [overlaps (map #(get ranges %) range-indices)
        len (:length (first overlaps))
        low (reduce (fn [acc range-low]
                      (if (<=str len acc range-low) acc range-low))
                    (map :low overlaps))
        high (reduce (fn [acc range-high]
                       (if (<=str len acc range-high) range-high acc))
                     (map :high overlaps))]
    (create-range (str low "-" high))))

(defn part2
  [input]
  (let [[ranges-str _] (str/split input #"\n\n")
        range-lines (str/split-lines ranges-str)
        ranges (mapv create-range range-lines)
        indexed-ranges (map-indexed vector ranges)
        overlap-groups (mapv (fn [[idx range]]
                               (conj
                                 (mapv #(first %) (filter #(overlaps-with? range (second %)) indexed-ranges))
                                 idx))
                             indexed-ranges)
        overlap-groups (merge-connected overlap-groups)
        ranges (map #(union-ranges ranges %) overlap-groups)]
    (reduce + (map (fn [{:keys [low high length same-part-len]}]
                       (if (= length same-part-len)
                         1
                         (+ 1 (- (bigint (subs high same-part-len)) (bigint (subs low same-part-len))))))
                   ranges))))

(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))