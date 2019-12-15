(ns aoc.05)

;; ### 1ST PART ###

;; First, you'll need to add two new instructions:

;; Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
;; Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.

(def input (slurp "inputs/05"))

(def intcode-seq
  (-> input
      (clojure.string/split #",")
      (->> (map #(Integer/parseInt %)))
      vec))

(defn- digits-coll [n]
  (loop [curr-num     n
         nums-as-coll '()]
    (if (= 0 curr-num)
      nums-as-coll
      (recur (quot curr-num 10) (conj nums-as-coll (rem curr-num 10))))))

(defn calc-param-vals [intcode cur-pos mode-p1 mode-p2]
  (let [ref1  (intcode (+ 1 cur-pos))
        ref2  (intcode (+ 2 cur-pos))
        dest  (intcode (+ 3 cur-pos))
        p1    (if (= 1 mode-p1) ref1 (intcode ref1))
        p2    (if (= 1 mode-p2) ref2 (intcode ref2))]
    [p1 p2 dest]))

(defn get-position [intcode operator cur-pos [p1 p2 _]]
  (if (operator 0 p1) p2 (+ 3 cur-pos)))

(defn change-modified [intcode modify cur-pos [p1 p2 dest]]
  (assoc intcode dest (modify p1 p2)))

(defn change-with-input [intcode position input]
  (let [dest (intcode (inc position))]
    (assoc intcode dest input)))

(defn change-compared [intcode comparer [p1 p2 dest]]
  (assoc intcode dest (if (comparer p1 p2) 1 0)))

(defn process [intcode pos input-num]
  (let [reversed-code (-> pos intcode digits-coll reverse)
        [opcode1 opcode2 param-mode1 param-mode2 _] reversed-code
        code (+ opcode1 (* 10 (if (nil? opcode2) 0 opcode2)))
        p-vals (when (contains? #{1 2 5 6 7 8} code)
                 (calc-param-vals intcode pos param-mode1 param-mode2))]
    (case code
      99 input-num
      1 (process (change-modified intcode + pos p-vals) (+ pos 4) 0)
      2 (process (change-modified intcode * pos p-vals) (+ pos 4) 0)
      3 (process (change-with-input intcode pos input-num) (+ pos 2) 0)
      4 (process intcode (+ pos 2) (-> pos inc intcode intcode))
      5 (process intcode (get-position intcode not= pos p-vals) 0)
      6 (process intcode (get-position intcode = pos p-vals) 0)
      7 (process (change-compared intcode < p-vals) (+ pos 4) 0)
      8 (process (change-compared intcode = p-vals) (+ pos 4) 0)
      :err)))

(println (process intcode-seq 0 5))

;;1
;; 13087969
;;2
;; 14110739

;; For example, here are several programs that take one input, compare it to the value 8, and then produce one output:

;; 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
;; 3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
;; 3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
;; 3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).

;; Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:

;; 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (using position mode)
;; 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)

;; TEST
;; (not= 1 (process [3,9,8,9,10,9,4,9,99,-1,8] 0 -10))
;; (= 1 (process [3,9,8,9,10,9,4,9,99,-1,8] 0 8))
;; (not= 1 (process [3,9,7,9,10,9,4,9,99,-1,8] 0 18))
;; (= 1 (process [3,9,7,9,10,9,4,9,99,-1,8] 0 -7))
;; (not= 1 (process [3,3,1108,-1,8,3,4,3,99] 0 -10))
;; (= 1 (process [3,3,1108,-1,8,3,4,3,99] 0 8))
;; (not= 1 (process [3,3,1107,-1,8,3,4,3,99] 0 31))
;; (= 1 (process [3,3,1107,-1,8,3,4,3,99] 0 -10))
;; (not= 1 (process [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 0))
;; (= 1 (process [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 10))
;; (not= 1 (process [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0 0))
;; (= 1 (process [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0 -10))

