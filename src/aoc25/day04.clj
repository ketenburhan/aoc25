(ns aoc25.day04
  (:require [clojure.string :as str]))

(def test-data "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.")
(def data (slurp "resources/aoc25/day04.txt"))

(defn transform-cell
  [cell]
  (if (= cell \@)
    1
    0))

(defn transform-cells
  [^String input]
  (mapv (fn [row]
          (mapv transform-cell row))
        (str/split-lines input)))

(defn matrix-combinations
  [x-list y-list]
  (mapcat identity (mapv (fn [row] (mapv (fn [col] [row col]) x-list)) y-list)))

(defn all-positions
  [table]
  (matrix-combinations (range (count (get table 0))) (range (count table))))

(defn accessible?
  [cell-rows [x y]]
  (if (= 1 (get (get cell-rows y) x))
    (let [height (count cell-rows)
          width (count (get cell-rows 0))
          top (max (dec y) 0)
          left (max (dec x) 0)
          bottom (min (+ y 2) height)
          right (min (+ x 2) width)
          horizontal (range top bottom)
          vertical (range left right)
          neighbors-and-itself (mapv (fn [row-col] (get-in cell-rows row-col)) (matrix-combinations vertical horizontal))]
      (< (dec (reduce + neighbors-and-itself)) 4))
    false))

(defn part1
  [input]
  (let [table (transform-cells input)]
    (count (filter true? (mapv #(accessible? table %) (all-positions table))))))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn patch-table
  [table remove-set]
  (mapv
    (fn [[y row]]
      (mapv (fn [[x item]] (if (get remove-set [x y])
                             0
                             item))
            (map-indexed vector row)))
    (map-indexed vector table)))

(defn part2
  [input]
  (let [input-table (transform-cells input)
        positions (all-positions input-table)]
    (loop [table input-table
           removed 0]
      (let [to-be-removed (apply hash-set (filter #(accessible? table %) positions))
            new-table (patch-table table to-be-removed)
            remove-count (count to-be-removed)]
        (if (= remove-count 0)
          (+ remove-count removed)
          (recur new-table (+ remove-count removed)))))))

(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))