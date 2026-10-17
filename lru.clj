(ns lru)

(defn- touch [order key]
  (conj (vec (remove #(= key %) order)) key))

(defn- touch-u [table order key]
  {:table table
   :order (touch order key)})

(defn- evict-u [table order key]
  {:table (dissoc table (first order))
   :order (touch (rest order) key)})

(defn lget [{:keys [table order] :as lru} key]
  (if-let [val (get table key)]
    [val (merge lru (touch-u table order key))]
    [nil lru]))

(defn lput [{:keys [cap table order] :as lru} key val]
  (let [updated (assoc table key val)]
    (if (< cap (count updated))
      (merge lru (evict-u updated order key))
      (merge lru (touch-u updated order key)))))

(defn make-lru [cap]
  {:cap cap
   :table {}
   :order []})

(def lru (volatile! (make-lru 3)))

(vswap! lru #(lput % :a 1))
(vswap! lru #(lput % :b 2))
(vswap! lru #(lput % :c 3))

(lget @lru :a)
(vswap! lru #(lput % :d 4))
