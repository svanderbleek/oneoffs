(ns bus
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest testing is]]))

(defn make-driver [route rumors]
  {:route (take 2 (cycle route)) :rumors rumors})

(defn move-drivers [world]
  (map #(update % :route rest) world))

(defn all-rumors [drivers]
  (apply set/union (map :rumors drivers)))

(defn share-rumors [drivers]
  (let [rumors (all-rumors drivers)]
    (map #(assoc % :rumors rumors) drivers)))

(defn spread-rumors [world]
  (->> (group-by (fn [driver] (first (:route driver))) world)
       (mapcat (fn [[_ drivers]] (share-rumors drivers)))))

(defn drive [world]
  (-> world move-drivers spread-rumors))

(defn all-shared [world]
  (apply = (map :rumors world)))

(defn track-spread [world]
  (loop [world (drive world)
         time 1]
    (cond
      (> time 480) :never
      (all-shared world) time
      :else (recur (drive world) (inc time)))))

(deftest acceptance-1
  (testing "passes acceptance test 1"
    (let [world [(make-driver [3 1 2 3] #{1})
                 (make-driver [3 2 3 1] #{2})
                 (make-driver [4 2 3 4 5] #{3})]]
      (is (= 6 (track-spread world))))))
