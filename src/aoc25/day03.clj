(ns aoc25.day03
  (:require [clojure.string :as str]))

(def test-data "987654321111111\n811111111111119\n234234234234278\n818181911112111")
(def data (slurp "resources/aoc25/day03.txt"))

(defn max-two-digit
  [bank]
  (let [max-first (char (apply max (map int (next (reverse bank)))))
        max-first-idx (str/index-of bank max-first)
        max-second (char (apply max (map int (subs bank (inc max-first-idx)))))]
    ;(println :bank bank :max-first max-first :max-second max-second :first-idx max-first-idx)
    (Integer/parseUnsignedInt (str max-first max-second))))

(defn part1
  [input]
  (reduce + (map max-two-digit (str/split-lines input))))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn max-one-digit
  [^String input slice]
  (let [char (char (apply max (map int (subs input (first slice) (second slice)))))
        idx (str/index-of input char (first slice))]
    {:char char :idx idx}))


(defn max-n-digit
  ([bank n] (max-n-digit bank n [0 (- (count bank) n)] []))
  ([input n range chars]
   (if (= n 0)
     (apply str chars)
     (let [digit (max-one-digit input range)
           new-n (dec n)
           new-chars (conj chars (:char digit))
           idx (:idx digit)
           new-range [(inc idx) (+ 1 (- (count input) new-n))]]
       ;(println digit new-n new-range new-chars)
       (recur input new-n new-range new-chars)))))

(defn part2
  [input]
  (reduce + (map #(bigint %) (map #(max-n-digit % 12) (str/split-lines input)))))


(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))