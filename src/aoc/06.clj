(ns aoc.06)

;; ### 1ST PART ###

;; In this visual representation, when two objects are connected by a line, the one on the right directly orbits the one on the left.

;; D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
;; L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
;; COM orbits nothing.

;; The total number of direct and indirect orbits in this example is 42.
;; What is the total number of direct and indirect orbits in your map data?

(def input (slurp "inputs/06"))

(def orbit-links
  (-> input
      clojure.string/split-lines
      (->> (map #(clojure.string/split % #"\)")))))

(defn orbited-pair [name]
  (first (filter #(= name (second %)) orbit-links)))

(defn calc-orbits-to-COM
  [arr name]
  (loop [n   name
         cnt 0]
    (if (= "COM" n)
      (inc cnt)
      (recur (first (orbited-pair n)) (inc cnt)))))

;; todo MEMOIZE???
(defn process-links [links]
  (loop [ls links
         cnt 0]
    (if (empty? ls)
      cnt
      (recur (rest ls) (+ cnt (calc-orbits-to-COM links (ffirst ls)))))))

(comment
  (process-links orbit-links))


;; ### 2ND PART ###

;; What is the minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting? (Between the objects they are orbiting - not between YOU and SAN.)


(defn calc-path
  [[n1 n2] path]
  (if (= "COM" n1)
    path
    (recur (orbited-pair n1) (conj path (vector n1 n2)))))

(defn distance [obj1 obj2 links]
  (let [start        (orbited-pair obj1)
        end          (orbited-pair obj2)
        start-to-COM (calc-path start [])
        end-to-COM   (calc-path end [])
        path-full    (-> (concat start-to-COM end-to-COM)
                         frequencies
                         vals)
        path-between (filter #(= 1 %) path-full)]
    (- (reduce + path-between) 2)))

(comment
  (distance "YOU" "SAN" orbit-links))
