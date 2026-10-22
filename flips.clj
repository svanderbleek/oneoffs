(ns flips)

(def ex [[1 0]
         [0 1]])

(defn h-flip [image]
  (mapv (fn [row] (vec (reverse row))) image))

(defn cols->rows [image]
  (let [rows (count image)
        cols (count (first image))]
    (->> (for [c (range cols)
               r (range rows)]
           (get-in image [r c]))
         (partitionv rows)
         (vec))))

(defn v-flip [image]
  (cols->rows (mapv (fn [col] (vec (reverse col))) (cols->rows image))))

(v-flip ex)

(defn rotate [image]
  (mapv (fn [col] (vec (reverse col))) (cols->rows image)))

(count (flatten (list (repeat 9 0) 1 (repeat (- 10 9 1) 0))))

(defn square [n]
  (vec (for [i (range n)]
         (vec (flatten (list (repeat i 0) 1 (repeat (- n i 1) 0)))))))

(clojure.pprint/pprint (square 10))
