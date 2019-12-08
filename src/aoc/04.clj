(ns aoc.04)

;; ### 1ST PART ###
;;

;; It is a six-digit number.
;; The value is within the range given in your puzzle input.
;; Two adjacent digits are the same (like 22 in 122345).
;; Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

;; Other than the range rule, the following are true:

;; 111111 meets these criteria (double 11, never decreases).
;; 223450 does not meet these criteria (decreasing pair of digits 50).
;; 123789 does not meet these criteria (no double).

;; How many different passwords within the range given in your puzzle input meet these criteria?

;; Your puzzle input is 264360-746325.

(defn increasing?
  [xs]
  (every? #(<= (first %) (second %)) (partition 2 1 xs)))

(defn same-near?
  [xs]
  (some #(= (first %) (second %)) (partition 2 1 xs)))

(defn ok?
  [num]
  (let [nums (map #(Character/digit % 10) (str num))]
    (and
     (increasing? nums)
     (same-near? nums))))

(let [from 264360
      to   746325]
  (count (filter ok? (range from (inc to)))))


;; ### 2ND PART ###
;; the two adjacent matching digits are not part of a larger group of matching digits.

;; Given this additional criterion, but still ignoring the range rule, the following are now true:

;; 112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
;; 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
;; 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).

(defn has-two-near?
  [xs]
  (some #(= 2 %) (vals (frequencies xs))))

(defn strict-ok?
  [num]
  (let [nums (map #(Character/digit % 10) (str num))]
    (and
     (increasing? nums)
     (has-two-near? nums)
     ;; (same-near? nums)
     )))

(let [from 264360
      to   746325]
  (count (filter strict-ok? (range from (inc to)))))
