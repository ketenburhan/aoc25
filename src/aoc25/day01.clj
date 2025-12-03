(ns aoc25.day01
  (:require [clojure.string :as str]))

(def data (slurp "resources/aoc25/day01.txt"))
(def test-data "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\n")

(defn row-to-num
  [row]
  (let [neg (str/starts-with? row "L")
        num (Integer/parseInt (apply str (next row)))]
    (if neg
      (* -1 num)
      num)))

(defn rotate
  [[old-pos _] input-row]
  (let [turn (row-to-num input-row)                         ; -520
        full-round-count (abs (int (/ turn 100)))           ; 5
        rest-turn (* (mod (abs turn) 100) (if (neg? turn) -1 1)) ; -20
        new-pos-raw (+ old-pos rest-turn)
        new-pos (mod new-pos-raw 100)
        new-zero-count (+ full-round-count (if (or (= old-pos 0)
                                                   (< 0 new-pos-raw 100))
                                             0
                                             1))]
    [new-pos new-zero-count]))

(defn part1
  [input]
  (let [lines (str/split-lines input)
        positions (reductions rotate [50 0] lines)
        zero-count (count (filter #(= 0 (first %)) positions))]
    zero-count))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn part2
  [input]
  (let [lines (str/split-lines input)
        positions (reductions rotate [50 0] lines)
        zero-count (reduce + (map second positions))]
    ; (doall (map println (mapv #(str (first (first %)) "\t" (second (first %)) "\t-> " (second %)) (map vector positions lines))))
    zero-count))

(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))

