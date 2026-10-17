(ns ds)

(defprotocol IStack
  (push [this item])
  (pekh [this])
  (poph [this]))

(extend-protocol IStack
  clojure.lang.PersistentVector
  (push [this item] (conj this item))
  (pekh [this] (last this))
  (poph [this] (when (seq this) (pop this))))

(defrecord LStack [list]
  IStack
  (push [_ item] (LStack. (cons item list)))
  (pekh [_] (first list))
  (poph [_] (LStack. (rest list))))

(defprotocol IQueue
  (enqueue [this item])
  (pkqueue [this])
  (dequeue [this]))

(extend-protocol IQueue
  clojure.lang.PersistentVector
  (enqueue [this item] (conj this item))
  (pkqueue [this] (first this))
  (dequeue [this] (rest this)))

(defrecord LQueue [front back]
  IQueue
  (enqueue [_ item] (LQueue. front (cons item back)))
  (pkqueue [_] (cond
                    (seq front) (first front)
                    (seq back) (last back)
                    :else nil))
  (dequeue [this] (cond
                    (seq front) (LQueue. (pop front) back)
                    (seq back) (LQueue. (pop (reverse back)) '())
                    :else this)))

(pekh (push (push [] 1) 2))
(pkqueue (enqueue (enqueue [] 1) 2))
(pekh (push (push (->LStack '()) 1) 2))
(pkqueue (enqueue (enqueue (->LQueue '() '()) 1) 2))
