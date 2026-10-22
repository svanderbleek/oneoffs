(ns serp
  (:require [quil.core :as q]))
(defn base-triangle [x y]
  (q/triangle (+ x 2) (- y 4)
              x       y
              (+ x 4) y))

(defn draw []
  (q/background 255)
  (q/stroke 0)
  (base-triangle 100 100))

;; can't translate algorithm from racket :(
(q/defsketch serpinski
  :size [400 400]
  :draw draw)
