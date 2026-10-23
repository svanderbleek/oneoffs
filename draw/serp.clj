(ns serp
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn base-triangle [a b c]
  (apply q/triangle (concat a b c)))

(defn sqr [x]
  (* x x))

(defn small? [[x1 y1] [x2 y2] _]
  (< (math/sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))) 5))

(defn mid-p [[x1 y1] [x2 y2]]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])

(defn serp [a b c]
  (if (small? a b c)
    nil
    (do
      (base-triangle a b c)
      (serp a (mid-p a b) (mid-p c a))
      (serp b (mid-p b c) (mid-p a b))
      (serp c (mid-p c a) (mid-p b c)))))

(defn draw []
  (q/background 255)
  (q/stroke 0)
  (serp [200 0] [0 400] [400 400]))

(q/defsketch serpinski
  :size [400 400]
  :draw draw)
