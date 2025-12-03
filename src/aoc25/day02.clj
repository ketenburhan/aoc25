(ns aoc25.day02
  (:require [clojure.string :as str]))

(def test-data "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
(def data (slurp "resources/aoc25/day02.txt"))

(defn invalid-id-part1?
  [^String id-str]
  (if (odd? (count id-str))
    false
    (let [pair (partition (int (/ (count id-str) 2)) id-str)]
      (= (first pair) (second pair)))))


(defn str-range
  [str-pair]
  (let [pair (str/split str-pair #"-")
        subs-pairs (map #(map (partial subs % 0) (reverse (range (inc (count %))))) pair)
        same-part (ffirst (filter (partial apply =) (apply map vector subs-pairs)))
        different-parts (map #(Integer/parseUnsignedInt %) (map #(subs % (count same-part)) pair))
        diff-range (range (first different-parts) (inc (second different-parts)))]
    (map #(str same-part %) diff-range)))

(defn part1
  [input]
  (let [input (str/trim-newline input)
        range-strs (str/split input #",")
        ranges (map #(str-range %) range-strs)
        invalid-ids (flatten (map #(filter invalid-id-part1? %) ranges))]
    (reduce + (map bigint invalid-ids))))


(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))


(defn invalid-id-part2?
  [^String id]
  (let [len (count id)
        half-len (int (/ len 2))
        partition-sizes (range 1 (inc half-len))
        partitions (map #(partition-all % id) partition-sizes)]
    (not-empty (filter (partial apply =) partitions))))

(defn part2
  [input]
  (let [input (str/trim-newline input)
        range-strs (str/split input #",")
        ranges (map #(str-range %) range-strs)
        invalid-ids (flatten (map #(filter invalid-id-part2? %) ranges))]
    (reduce + (map bigint invalid-ids))))


(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))