(ns words
  (:require [clojure.string :as str]))

(defonce words (set (str/split-lines (slurp "/usr/share/dict/words"))))

(defn count-by-letter [words]
  (frequencies (map first words)))

(reverse (sort-by val (count-by-letter words)))

(defn words-by-first-letter [words]
  (update-vals (group-by first words) set))

(words-by-first-letter words)

(cons '(1 2) '((1 2)))

(defn insert-permutation [permutation item]
  (if (empty? permutation)
    (list (list item))
    (concat (list (cons item permutation))
            (map #(cons (first permutation) %) (insert-permutation (rest permutation) item)))))

(defn permute-lst [coll]
  (if (empty? coll)
    (list '())
    (mapcat #(insert-permutation % (first coll)) (permute-lst (rest coll)))))

(defn permute-rec [[x & xs]]
  (if (empty? xs)
    [[x]]
    (concat (map #(cons (first xs) %) (permute-rec (cons x (rest xs))))
            (map #(cons x %) (permute-rec xs)))))

;; best so far
(defn permute-lazy [coll]
  (if-not (next coll)
    (list coll)
    (lazy-seq (concat (map #(cons (second coll) %) (permute-lazy (cons (first coll) (rest (rest coll)))))
                      (map #(cons (first coll) %) (permute-lazy (rest coll)))))))

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

(defn remove-first [coll x]
  (loop [removed []
         remain coll]
    (if (empty? remain)
      removed
      (if (= (first remain) x)
        (recur (into removed (rest remain)) [])
        (recur (conj removed (first remain)) (rest remain))))))

;; comparable, or better than lazy
(defn permute-for [coll]
  (if (empty? coll)
    '(())
    (for [x         coll
          without-x (permute-for (remove-first coll x))]
      (cons x without-x))))

(time (permute-for (range 10)))
(time (permute-lazy (range 10)))
(time (permute-rec (range 10)))
(time (permute-lst (range 10)))

(defn alt-words [word]
  (->> (permute-lst word)
       (map #(apply str %))
       (filter words)
       (remove #(= word %))
       (distinct)))

(alt-words "add")
