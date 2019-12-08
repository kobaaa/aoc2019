(ns aoc.03)

;; ### 1ST PART ###
;;
;;     R75,D30,R83,U83,L12,D49,R71,U7,L72
;;     U62,R66,U55,R34,D71,R55,D58,R83
;;     = distance 159
;;
;;     R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;;     U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
;;     = distance 135
;;
;; What is the Manhattan distance from the central port to the closest intersection?
;; (def input-lines ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])

(def input (slurp "inputs/03"))
(def input-lines (clojure.string/split input #"\n"))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn get-commons [l1 l2]
  (let [f1 (frequencies l1)
        f2 (frequencies l2)
        common (clojure.set/intersection (set l1) (set l2))
        love (merge-with (fn [val1 val2] (min val1 val2)) f1 f2)]
    (mapcat #(repeat (love %) %) common)))

(defn directions [line]
  (map (juxt first parse-int) (clojure.string/split line #",")))

(defn points [[hor ver] [direction num]]
  (let [qty (range 1 (inc num))]
    (case direction
      \R (into [] (map #(vector (+ hor %) ver) qty))
      \L (into [] (map #(vector (- hor %) ver) qty))
      \U (into [] (map #(vector hor (+ ver %)) qty))
      \D (into [] (map #(vector hor (- ver %)) qty)))))

(defn collect-coords [p]
  (loop [path p
         pts [[0 0]]]
    (if (empty? path)
      (rest pts)
      (recur (rest path) (into pts (points (last pts) (first path)))))))

(let [line1 (first input-lines)
      line2 (second input-lines)
      path1 (directions line1)
      path2 (directions line2)]
  (->>
   (get-commons (collect-coords path1) (collect-coords path2))
   (map #(+ (Math/abs (first %)) (Math/abs (second %))))
   (apply min)))


;; ### 2ND PART ###

(defn collect-coords-from-start [p]
  (loop [path p
         pts [[0 0]]]
    (if (empty? path)
      pts
      (recur (rest path) (into pts (points (last pts) (first path)))))))

(let [line1 (first input-lines)
      line2 (second input-lines)
      path1 (directions line1)
      path2 (directions line2)
      wire1 (collect-coords-from-start path1)
      wire2 (collect-coords-from-start path2)]
  (->>
   (get-commons wire1 wire2)
   (map #(+ (.indexOf wire1 %) (.indexOf wire2 %)))
   (filter pos?)
   (apply min)))
