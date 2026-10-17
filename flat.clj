(ns flat)

(defn flat [list]
  (loop [fs []
         [x & xs :as l] list]
    (cond
      (empty? l) fs
      (sequential? x) (recur fs (into x xs))
      :else (recur (conj fs x) xs))))
