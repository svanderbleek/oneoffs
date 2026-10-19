(ns bus
  (:require [clojure.set :as set]))

(defn make-driver [route rumors]
  {:route (take 2 (cycle route)) :rumors rumors})

(defn move-driver [driver]
  (update driver :route rest))

(defn move-drivers [world]
  (map move-driver world))

(defn all-rumors [drivers]
  (apply set/union (map :rumors drivers)))

(defn update-rumors [drivers]
  (let [rumors (all-rumors drivers)]
    (println rumors)
    (map #(assoc % :rumors rumors) drivers)))

(defn spread-rumors [world]
  (->> (group-by (fn [driver] (first (:route driver))) world)
       (mapcat (fn [[_ drivers]] (update-rumors drivers)))))

(defn drive [world]
  (-> world move-drivers spread-rumors))

(spread-rumors [(make-driver [:a] #{:a}) (make-driver [:b] #{:b})])

(mapcat (fn [[_ vs]] vs) (group-by :r [{:r 1} {:r 2}]))
