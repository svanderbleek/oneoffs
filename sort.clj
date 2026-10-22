(ns sort)

(defn s-merge [xs ys]
  (loop [merged                    []
         [x & more-x :as remain-x] xs
         [y & more-y :as remain-y] ys]
    (cond
      (empty? remain-x) (concat merged remain-y)
      (empty? remain-y) (concat merged remain-x)
      :else (if (<= x y)
              (recur (conj merged x) more-x remain-y)
              (recur (conj merged y) remain-x more-y)))))

(defn q-split [x xs]
  ((juxt #(get % true) #(get % false)) (group-by #(> x %) xs)))

(defn q-sort [xs]
  (cond
    (empty? xs)     []
    (not (next xs)) xs
    :else (let [[x & more]       xs
                [smaller larger] (q-split x more)]
            (concat (q-sort smaller) [x] (q-sort larger)))))
