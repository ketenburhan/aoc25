(ns aoc25.day06
  (:require [clojure.string :as str]))

(def test-data "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ")
(def data (slurp "resources/aoc25/day06.txt"))

(defn get-column
  [rows col-idx]
  (mapv (fn [row] (get row col-idx)) rows))

(defn run-column
  [operators num-rows col-idx]
  (let [op (get operators col-idx)
        op (condp = op
             "+" +
             "*" *)
        col (get-column num-rows col-idx)]
    (apply op col)))
(defn part1
  [input]
  (let [rows (str/split-lines input)
        rows (mapv #(str/trim %) rows)
        operators (str/split (peek rows) #"\s+")
        num-rows (map #(str/split % #"\s+") (pop rows))
        num-rows (mapv (fn [row] (mapv #(Integer/parseUnsignedInt %) row)) num-rows)]
    (reduce + (map #(run-column operators num-rows %) (range (count operators))))))

(println (str "part1 test: " (part1 test-data)))
(println (str "part1 output: " (part1 data)))

(defn get-operations-data
  [op-row]
  ; FACT: Spaces after arithmetic operator equals to digits of that operation.
  ; But the last operation does not follow this rule, instead it's (spaces+1).
  (let [matcher (re-matcher #"([+*])(\s+)" op-row)
        op-data (loop [acc []]
                  (let [match (re-find matcher)]
                    (if (nil? match)
                      acc
                      (recur (conj acc {:op     (second match)
                                        :digits (count (nth match 2))})))))
        ; digits: (spaces+1) for last operation
        op-data (conj (pop op-data) (update (peek op-data) :digits (fn [digits] (inc digits))))]
    op-data))

(defn calculate-column
  [num-rows op-data ^Number cursor]
  (let [{:keys [op digits]} op-data
        digit-range (range cursor (+ cursor digits))
        nums (map (fn [cursor] (-> (map (fn [row] (subs row cursor (inc cursor))) num-rows)
                                   str/join
                                   str/trim
                                   Integer/parseUnsignedInt))
                  digit-range)
        op (condp = op
             "+" +
             "*" *)]
    (apply op nums)))

(defn calculate-and-sum-columns
  [num-rows operations-data]
  (loop [sum 0
         ops operations-data
         cursor 0]
    (let [op-data (first ops)
          next-cursor (+ cursor (:digits op-data) 1)
          calculated (calculate-column num-rows op-data cursor)
          ops-left (rest ops)]
      (if (empty? ops-left)
        (+ calculated sum)
        (recur (+ calculated sum) ops-left next-cursor)))))

(defn part2
  [input]
  (let [rows (str/split-lines input)
        operations-data (get-operations-data (peek rows))
        num-rows (pop rows)]
    (calculate-and-sum-columns num-rows operations-data)))


(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))