(ns aoc25.day08
  (:require [clojure.string :as str]))

(def test-data "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689")
(def data (slurp "resources/aoc25/day08.txt"))

(defrecord Vec3 [x y z])
(defrecord DistanceOf [^Integer i ^Integer j ^Double dist])



(defn str->Vec3
  [input]
  (apply ->Vec3 (map #(Integer/parseInt %) (str/split input #","))))

(comment (str->Vec3 "162,817,812"))

(defn all-combinations
  [vectors]
  (mapcat identity (loop [vecs vectors
                          pairs []]
                     (let [curr-vec (first vecs)
                           rst (rest vecs)]
                       (if (empty? rst)
                         pairs
                         (recur rst (conj pairs (map #(vector curr-vec %) rst))))))))


(comment (all-combinations [(->Vec3 1 2 3)
                            (->Vec3 4 5 6)
                            (->Vec3 7 8 9)
                            (->Vec3 10 11 12)]))

(defn all-combinations
  [items]
  (mapcat identity (loop [coll items
                          pairs []]
                     (let [curr (first coll)
                           rst (rest coll)]
                       (if (empty? rst)
                         pairs
                         (recur rst (conj pairs (map #(vector curr %) rst))))))))

(comment (all-combinations [1 2 3 4]))

(defn distance
  [^Vec3 v1 ^Vec3 v2]
  (let [dist-x (- (:x v1) (:x v2))
        dist-y (- (:y v1) (:y v2))
        dist-z (- (:z v1) (:z v2))]
    (Math/sqrt (+ (Math/pow dist-x 2)
                  (Math/pow dist-y 2)
                  (Math/pow dist-z 2)))))


(defrecord ConnectionMap [idx-set-map set-idxs-map])

(defn create-connection-map
  [items]
  (let [idx-set (apply hash-map (mapcat identity (map #(vector % %) items)))
        set-idxs (apply hash-map (mapcat identity (map #(vector % [%]) items)))]
    (->ConnectionMap idx-set set-idxs)))

(comment (create-connection-map (range 3)))

;; assuming sets of i and j are different 
(defn join-sets-of
  [m i j]
  (let [set-of-i (get-in m [:idx-set-map i])
        set-of-j (get-in m [:idx-set-map j])
        idxs-of-set-of-i (get-in m [:set-idxs-map set-of-i])
        idxs-of-set-of-j (get-in m [:set-idxs-map set-of-j])
        m (reduce (fn [acc idx-from-set-of-j] (assoc-in acc [:idx-set-map idx-from-set-of-j] set-of-i)) m idxs-of-set-of-j)
        m (assoc-in m [:set-idxs-map set-of-j] [])
        m (assoc-in m [:set-idxs-map set-of-i] (apply conj idxs-of-set-of-i idxs-of-set-of-j))]
    m))

(comment (-> (create-connection-map (range 6))
             (join-sets-of 3 4)
             (join-sets-of 1 3)))

(defn make-shortest-connections
  [distance-ofs items wire-cap]
  (loop [dist-ofs distance-ofs
         connection-map (create-connection-map items)
         wires 0]
    (if (or (= wires wire-cap) (empty? dist-ofs))
      connection-map
      (let [{:keys [i j]} (first dist-ofs)
            rst (rest dist-ofs)]
        (if (not= (get-in connection-map [:idx-set-map i]) (get-in connection-map [:idx-set-map j]))
          (recur rst (join-sets-of connection-map i j) (inc wires))
          (recur rst connection-map (inc wires)))))))

(defn part1
  [input num-wires]
  (let [rows (str/split-lines input)
        vectors (mapv str->Vec3 rows)
        idx-range (range (count vectors))
        combinations (all-combinations idx-range)
        distances (mapv (fn [[i j]]
                          (->DistanceOf i
                                        j
                                        (distance (get vectors i)
                                                  (get vectors j))))
                        combinations)
        sorted (sort-by :dist distances)
        connection-map (make-shortest-connections sorted idx-range num-wires)
        circuits (:set-idxs-map connection-map)]
    (reduce * (take 3 (sort > (map count (map second circuits)))))))



(println (str "part1 test: " (part1 test-data 10)))
(println (str "part1 output: " (part1 data 1000)))


(defn make-shortest-connections-part2
  [distance-ofs items]
  (loop [dist-ofs distance-ofs
         connection-map (create-connection-map items)]
    (let [{:keys [i j]} (first dist-ofs)
          rst (rest dist-ofs)]
      (if (not= (get-in connection-map [:idx-set-map i]) (get-in connection-map [:idx-set-map j]))
        (let [new-connection-map (join-sets-of connection-map i j)]
          (if (= 1 (count (filter seq (map second (:set-idxs-map new-connection-map)))))
            [i j]
            (recur rst new-connection-map)))
        (recur rst connection-map)))))

(defn part2
  [input]
  (let [rows (str/split-lines input)
        vectors (mapv str->Vec3 rows)
        idx-range (range (count vectors))
        combinations (all-combinations idx-range)
        distances (mapv (fn [[i j]]
                          (->DistanceOf i
                                        j
                                        (distance (get vectors i)
                                                  (get vectors j))))
                        combinations)
        sorted (sort-by :dist distances)
        last-connection (make-shortest-connections-part2 sorted idx-range)
        last-connection (mapv #(get vectors %) last-connection)]
    (reduce * (map :x last-connection))))

(println (str "part2 test: " (part2 test-data)))
(println (str "part2 output: " (part2 data)))
