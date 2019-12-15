(ns aoc.08)

;; ### 1ST PART ###

;; For example, given an image 3 pixels wide and 2 pixels tall, the image data 123456789012 corresponds to the following image layers:

;; Layer 1: 123
;; 456

;; Layer 2: 789
;; 012

;; The image you received is 25 pixels wide and 6 pixels tall.

;; To make sure the image wasn't corrupted during transmission, the Elves would like you to find the layer that contains the fewest 0 digits. On that layer, what is the number of 1 digits multiplied by the number of 2 digits?

(def input (slurp "inputs/08"))

(def layers (partition 6 (partition 25 input)))

(defn analyze-layer [l]
  (let [frs     (frequencies (flatten l))
        zeros   (frs \0)
        ones    (frs \1)
        twos    (frs \2)]
    {:zs  zeros
     :sum (* ones twos)}))

(->> layers
     (map analyze-layer)
     (sort-by first)
     first
     :sum)


;; ### 2ND PART ###
;; The image is rendered by stacking the layers and aligning the pixels with the same positions in each layer. The digits indicate the color of the corresponding pixel: 0 is black, 1 is white, and 2 is transparent.
;; The layers are rendered with the first layer in front and the last layer in back. So, if a given position has a transparent pixel in the first and second layers, a black pixel in the third layer, and a white pixel in the fourth layer, the final image would have a black pixel at that position.

(defn define-color [xs]
  (let [val (-> (drop-while #(= \2 %) xs)
                first)]
    (if (not= val \1) " " val)))

(defn decode [lrs]
  (let [cnt (count lrs)]
    (loop [ls lrs
           final ""]
      (if (empty? (flatten ls))
        final
        (recur (map rest ls) (str final
                                  (->> (map first ls)
                                       (apply interleave)
                                       (partition cnt)
                                       (map define-color)
                                       (apply str))
                                  "\n"))))))

(println
 (decode layers))
