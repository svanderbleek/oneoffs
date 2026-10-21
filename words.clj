(ns words)

(defn permute-rec [[x & xs]]
  (if (empty? xs)
    [[x]]
    (concat (map #(cons (first xs) %) (permute-rec (cons x (rest xs))))
            (map #(cons x %) (permute-rec xs)))))

(defn permute-lazy [])

(defn remove-once [x v]
  (let [i (.indexOf v x)]
    (into (subvec v 0 i)
          (subvec v (inc i)))))

(defn advance-permutation [[permutation remaining]]
  (if (empty? remaining)
    permutation
    (mapv (fn [r] [(conj permutation r) (remove-once r remaining)]) remaining)))

;; super slow and broken lol
(defn permute-tco [coll]
  (loop [permutations [[[] coll]]
         iterations (count coll)]
    (if (zero? iterations)
      (mapv advance-permutation permutations)
      (recur (vec (mapcat advance-permutation permutations)) (dec iterations)))))

(time (permute-tco (vec (range 4))))
(time (permute-rec (range 4)))

(defn alt-words [word]
  (map #(apply str %) (permute-rec word)))

(alt-words "rats")
