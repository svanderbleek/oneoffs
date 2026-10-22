(ns graph)

(def ex1
  {:A [:B :E]
   :B [:E :F]
   :C [:D]
   :E [:C :F]
   :F [:D :G]
   :G [:A]})

(defn children [node graph]
  (node graph))

(defn add-children [queue path nodes]
  (reduce (fn [q p] (conj q p))
          queue
          (map #(conj path %) nodes)))

(defn find-path [from to graph]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from])
         seen  #{}]
    (if (empty? queue)
      nil
      (let [path (peek queue)
            more (pop queue)
            node (last path)]
        (cond
          (= node to) path
          (contains? seen node) (recur more seen)
          :else (recur (add-children more path (children node graph))
                       (conj seen node)))))))

(find-path :A :Z ex1)
