(ns recipes)


(def recipes-list
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

(defn all-ways
  ([recipes pantry]
   (all-ways (first recipes) (rest recipes) pantry []))
  ([recipe recipes pantry used]
   (if-not recipe
     (if (empty? used)
       nil
       [used])
     (concat (when-let [used-pantry (use-ingredients pantry recipe)]
               (all-ways recipe recipes used-pantry (into used recipe)))
             (all-ways (first recipes) (rest recipes) pantry used)))))

(count (all-ways recipes-list pantry-start)) ;; 431
