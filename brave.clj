(ns brave)

(defn mapr [f xs]
  (reduce (fn [fxs x]
            (conj fxs (f x)))
          []
          xs))

(mapr inc [1 2 3])

(defn filr [p xs]
  (reduce (fn [pxs x]
            (if (p x)
              (conj pxs x)
              pxs))
          []
          xs))

(filr even? [1 2 3])

(defn somr [p xs]
  (reduce (fn [s x]
            (or (p x) s))
          nil
          xs))

(somr even? [1 3 3 4])

(def food-journal
  [{:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 2.0}
   {:month 1 :day 2 :human 5.2 :critter 2.1}
   {:month 1 :day 2 :human 5.3 :critter 2.2}
   {:month 2 :day 1 :human 4.9 :critter 2.1}
   {:month 2 :day 2 :Human 5.0 :critter 2.5}
   {:month 3 :day 1 :human 4.2 :critter 3.3}
   {:month 3 :day 2 :human 4.0 :critter 3.8}
   {:month 4 :day 1 :human 3.7 :critter 3.9}
   {:month 4 :day 2 :human 3.7 :critter 3.6}])

(map (fn [[_ v]] (count v)) (group-by (juxt :month :day) food-journal))

(defn my-comp [& fs]
  (let [fs (reverse fs)]
    (fn [& args]
        (reduce (fn [x f] (f x))
                (apply (first fs) args)
                (rest fs)))))

((my-comp int inc #(/ % 2) :intr :attr) {:attr {:intr 10}})
