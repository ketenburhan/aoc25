(ns aoc25.day07
  (:require [clojure.string :as str]))

(def test-data ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............")
(def data (slurp "resources/aoc25/day07.txt"))


(defn get-splitter-pos
  [row]
  (loop [acc []
         cursor 0]
    (if-let [index (str/index-of row "^" cursor)]
      (recur (conj acc index) (inc index))
      acc)))

; (get-splitter-pos (first (next (next (str/split-lines test-data)))))

(defn available-streams
  [splitters ^Number start-pos]
  (loop [available-streams [start-pos]
         row-index 0
         splits []]
    (if-let [current-splitters (get splitters row-index)]
      (let [groups (group-by #(if (.contains current-splitters %) :split :no-split) available-streams)
            new-streams (flatten (map #(vector (dec %) (inc %)) (:split groups)))
            new-available-streams (apply conj (:no-split groups) new-streams)
            new-available-streams (distinct new-available-streams)]
        (recur new-available-streams (inc row-index) (conj splits (get groups :split []))))
      {:available-streams available-streams :splits splits})))


(defn part1
  [input]
  (let [rows (str/split-lines input)
        start-pos (str/index-of (first rows) "S")
        splitters (mapv get-splitter-pos rows)]
    (count (flatten (:splits (available-streams splitters start-pos))))))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn count-per-stream
  [streams]
  (loop [acc {}
         list streams]
    (let [[pos len] (first list)
          rst (rest list)
          old-value (get acc pos 0)
          new-acc (assoc acc pos (+ old-value len))]
      (if (empty? rst)
        new-acc
        (recur new-acc rst)))))

(comment (count-per-stream [[4 1] [6 1] [6 2] [6 1] [10 1]]))
; => {4 1, 6 4, 10 1}


(defn available-streams-part2
  [splitters ^Number start-pos]
  (loop [available-streams [[start-pos 1]]
         row-index 0
         splits []]
    (if-let [current-splitters (get splitters row-index)]
      (let [groups (group-by #(if (.contains current-splitters (first %)) :split :no-split) available-streams)
            new-streams (mapcat identity (mapv (fn [[pos times]] [[(dec pos) times] [(inc pos) times]]) (:split groups)))
            new-available-streams (apply conj (get groups :no-split []) new-streams)
            new-available-streams (count-per-stream new-available-streams)]
        (recur new-available-streams (inc row-index) (conj splits (get groups :split []))))
      {:available-streams available-streams :splits splits})))


(defn part2
  [input]
  (let [rows (str/split-lines input)
        start-pos (str/index-of (first rows) "S")
        splitters (mapv get-splitter-pos rows)]
    (reduce + (map second (:available-streams (available-streams-part2 splitters start-pos))))))

(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))