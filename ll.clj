(ns ll)

(definterface INode
  (gval [])
  (gnxt [])
  (sval [value])
  (snxt [node]))

(deftype Node [^:volatile-mutable val ^:volatile-mutable ^INode nxt]
  INode
  (gval [_] val)
  (gnxt [_] nxt)
  (sval [this value] (set! val value) this)
  (snxt [this node] (set! nxt node) this))

(def lst (Node. "1" (Node. "2" (Node. "3" nil))))

;; in place reverse
(defn lrev [hed]
  (loop [prv nil
         cur hed
         nxt (.gnxt hed)]
    (if-not nxt
      (.snxt cur prv)
      (do
        (.snxt cur prv)
        (recur cur nxt (.gnxt nxt))))))

(def lrv (lrev lst))

(.. lrv gnxt gnxt gval)
