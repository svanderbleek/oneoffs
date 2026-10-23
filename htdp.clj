(ns htdp)

(def rel-ex '(50 40 70 30 30))

(reductions (fn [a x] (+ a x)) rel-ex)
