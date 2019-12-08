(ns aoc.02)

;; ### 1ST PART ###
;;
;; 99 means that the program is finished and should immediately halt. Encountering an unknown opcode means something went wrong.

;; Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.

;; For example, if your Intcode computer encounters 1,10,20,30, it should read the values at positions 10 and 20, add those values, and then overwrite the value at position 30 with their sum.

;; Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.

;; Once you're done processing an opcode, move to the next one by stepping forward 4 positions.

;; 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
;; 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
;; 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
;; 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

;; Once you have a working computer, the first step is to restore the gravity assist program (your puzzle input) to the "1202 program alarm" state it had just before the last computer caught fire. To do this, before running the program, replace position 1 with the value 12 and replace position 2 with the value 2. What value is left at position 0 after the program halts?


(def input (slurp "inputs/02"))
(def replace-num1 12)
(def replace-num2 2)

(defn restored-nums [n1 n2]
  (-> input
      (clojure.string/split #",")
      (->> (map #(Integer/parseInt %)))
      vec
      (assoc 1 n1 2 n2)))

(defn change [intcode modify position]
  (let [n1   (intcode (+ 1 position))
        n2   (intcode (+ 2 position))
        dest (intcode (+ 3 position))]
    (assoc intcode dest (modify (intcode n1) (intcode n2)))))

(defn process [intcode cur-pos]
  (let [next-pos (+ 4 cur-pos)]
    (case (intcode cur-pos)
      99 intcode
      1 (process (change intcode + cur-pos) next-pos)
      2 (process (change intcode * cur-pos) next-pos)
      :err)))

(first (process (restored-nums replace-num1 replace-num2) 0))


;; ### 2ND PART ###
;;
;; Find the input noun and verb that cause the program to produce the output 19690720. What is 100 * noun + verb? (For example, if noun=12 and verb=2, the answer would be 1202.)

(def target 19690720)

;; List comprehension
;; (for [x [0 1 2 3 4 5]
;;       :let [y (* x 3)]
;;       :when (even? y)]
;;   y)

(defn determine-for-given-target [t]
  (for [noun (range 100)
        verb (range 100)
        :let [output (first (process (restored-nums noun verb) 0))]
        :when (= t output)]
    (+ (* 100 noun) verb)))

(determine-for-given-target target)

