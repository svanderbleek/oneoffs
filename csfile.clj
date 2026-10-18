(ns csfile
  (:require [clojure.string :as str]))

(def rows (map #(str/split % #",") (str/split (slurp "f.csv") #"\n")))

(def out (filter #(< 3 (Integer/parseInt (:val %))) (map #(zipmap [:name :val] %) rows)))

(spit "o.csv" (str (str/join "\n" (map #(str/join "," (vals %)) out)) "\n"))
