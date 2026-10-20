(ns recipes)

(def recipes-all
  "A map of recipe name -> ingredients needed to make the recipe.
  Note that ingredients are normalized as integer values.
  We don't care about specific units."
  {:apple-pie              {:flour 1 :apples 2 :sugar 1 :butter 1}
   :pecan-pie              {:flour 1 :sugar 1 :butter 1 :nuts 1}
   :chocolate-pie          {:flour 1 :chocolate 2 :sugar 1 :butter 1}
   :chocolate-chip-cookies {:flour 1 :chocolate 2 :sugar 1 :butter 2}
   :buttery-cookies        {:flour 2 :butter 3}
   :sugar-cookies          {:flour 1 :sugar 3}
   :bread                  {:flour 3 :salt 1}})

(def pantry-start
  "The amount of normalized units I have in my pantry."
  {:flour     15
   :butter    5
   :chocolate 5
   :apples    2
   :nuts      2
   :salt      3
   :sugar     8})

(defn ingredients [recipe]
  (second recipe))

(defn real? [pantry]
  (every? #(<= 0 %) (vals pantry)))

(defn use-ingredients [pantry recipe]
  (let [used-pantry (merge-with - pantry (ingredients recipe))]
    (if (real? used-pantry)
      used-pantry
      nil)))

(defn all-ways-rc
  ([recipes pantry]
   (all-ways-rc (first recipes) (rest recipes) pantry []))
  ([recipe recipes pantry used]
   (if-not recipe
     (when-not (some #(use-ingredients pantry %) recipes-all) [used])
     (concat (when-let [used-pantry (use-ingredients pantry recipe)]
               (all-ways-rc recipe recipes used-pantry (into used recipe)))
             (all-ways-rc (first recipes) (rest recipes) pantry used)))))

(time (all-ways-rc recipes-all pantry-start))

(defn name-of [recipe]
  (first recipe))

(defn use-ingredients-g [pantry recipe]
  (let [used-pantry (merge-with - pantry (ingredients recipe))]
    (if (real? used-pantry)
      [(name-of recipe) used-pantry]
      nil)))

(defn children [pantry recipes]
  (keep #(use-ingredients-g pantry %) recipes))

(defn all-ways-g [recipes pantry]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [[] pantry])
         seen  #{}]
    (if (empty? queue)
      seen
      (let [[path path-pantry] (peek queue)
             popped-queue      (pop queue)
             next (children path-pantry recipes)]
        (if (empty? next)
          (recur popped-queue (conj seen (frequencies path)))
          (recur (reduce (fn [updated-queue [recipe recipe-pantry]]
                           (conj updated-queue [(conj path recipe) recipe-pantry]))
                         popped-queue
                         next)
                 seen))))))

(time (all-ways-g recipes-all pantry-start)) ;; very slow :(
