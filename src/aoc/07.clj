(ns aoc.07)

(def input (slurp "inputs/07"))

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
  (println position input)
  (let [dest (intcode (inc position))]
    (assoc intcode dest input)))

(defn change-compared [intcode comparer [p1 p2 dest]]
  (assoc intcode dest (if (comparer p1 p2) 1 0)))

(defn process [intcode pos in-nums]
  ;; (println input-nums "pos:" pos)
  ;; (println (map-indexed vector intcode))
  (let [;; input-num (first input-nums)
        ;; in-nums input-nums

        reversed-code (-> pos intcode digits-coll reverse)
        [opcode1 opcode2 param-mode1 param-mode2 _] reversed-code
        code (+ opcode1 (* 10 (if (nil? opcode2) 0 opcode2)))
        p-vals (when (contains? #{1 2 5 6 7 8} code)
                 (calc-param-vals intcode pos param-mode1 param-mode2))]
    (case code
      99 (first in-nums)
      1 (process (change-modified intcode + pos p-vals) (+ pos 4) in-nums)
      2 (process (change-modified intcode * pos p-vals) (+ pos 4) in-nums)
      3 (process (change-with-input intcode pos (first in-nums)) (+ pos 2) (rest in-nums))
      4 (process intcode (+ pos 2) (conj in-nums (-> pos inc intcode intcode)))
      5 (process intcode (get-position intcode not= pos p-vals) in-nums)
      6 (process intcode (get-position intcode = pos p-vals) in-nums)
      7 (process (change-compared intcode < p-vals) (+ pos 4) in-nums)
      8 (process (change-compared intcode = p-vals) (+ pos 4) in-nums)
      :err)))

;; (println (process intcode-seq 0 [0 5]))


;; ### 1ST PART ###

(def phases
  (for [a (range 5)
        b (range 5)
        c (range 5)
        d (range 5)
        e (range 5)
        :let [xs  [a b c d e]]
        :when (= 5 (count (set xs)))] xs))

(defn calc-phase [p]
  (loop [phase-settings p
         out-num        0]
    (if (empty? phase-settings)
      out-num
      (recur (rest phase-settings)
             (process intcode-seq 0 [(first phase-settings) out-num])))))

(def res-part1
  (->> phases
       (map calc-phase)
       (apply max)))

(println res-part1)


;; ### 2ND PART ###

;; (def phases2
;;   (for [a (range 5 10)
;;         b (range 5 10)
;;         c (range 5 10)
;;         d (range 5 10)
;;         e (range 5 10)
;;         :let [xs  [a b c d e]]
;;         :when (= 5 (count (set xs)))] xs))

;; ;;Max thruster signal 139629729
;; (def sett2 [9,8,7,6,5])

;; (def tz2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
;;           27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

;; (defn process2 [intcode pos in-nums]
;;   ;; (println input-nums "pos:" pos)
;;   ;; (println (map-indexed vector intcode))
;;   (let [
;;         reversed-code (-> pos intcode digits-coll reverse)
;;         [opcode1 opcode2 param-mode1 param-mode2 _] reversed-code
;;         code (+ opcode1 (* 10 (if (nil? opcode2) 0 opcode2)))
;;         p-vals (when (contains? #{1 2 5 6 7 8} code)
;;                  (calc-param-vals intcode pos param-mode1 param-mode2))]
;;     (println code "pos" pos "incode" (map-indexed vector intcode))
;;     (case code
;;       99 (first in-nums)
;;       1 (process2 (change-modified intcode + pos p-vals) (+ pos 4) in-nums)
;;       2 (process2 (change-modified intcode * pos p-vals) (+ pos 4) in-nums)
;;       3 (process2 (change-with-input intcode pos (first in-nums)) (+ pos 2) (rest in-nums))
;;       4 [intcode (+ pos 2) (conj in-nums (-> pos inc intcode intcode))]
;;       5 (process2 intcode (get-position intcode not= pos p-vals) in-nums)
;;       6 (process2 intcode (get-position intcode = pos p-vals) in-nums)
;;       7 (process2 (change-compared intcode < p-vals) (+ pos 4) in-nums)
;;       8 (process2 (change-compared intcode = p-vals) (+ pos 4) in-nums)
;;       :err)))

;; (process2 tz2 0 [9 0])

;; (defn calc-phase2 [p]
;;   (loop [phase-settings p
;;          out-num        0]
;;     (if (empty? phase-settings)
;;       out-num
;;       (recur (rest phase-settings)
;;              (process tz2 0 [(first phase-settings) out-num])))))

;; (calc-phase2 sett2)






;; ##############

;; (def phasesq [4,3,2,1,0])
;; (def tzt [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
;; ;;54321
;; (def tzt2 [ 0,1,2,3,4])
;; (def tzt [
;;           3,23,3,24,1002,24,10,24,1002,23,-1,23,
;;           101,5,23,23,1,24,23,23,4,23,99,0,0
;;           ])
;; ;; Max thruster signal 65210
;; (def tzt3 [1,0,4,3,2])
;; (def tzt [
;;           3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
;;           1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
;;           ])
;; ;; Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
;; (process [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 0 [4 0])
;; (process [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 0 [3 4])
;; (process [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 0 [2 43])
;; (process [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 0 [1 432])
;; (process [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] 0 [0 4321])
