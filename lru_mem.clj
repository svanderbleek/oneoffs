(ns lru-mem)

(defprotocol IDLNode
  (gnval [this])
  (gprev [this])
  (gnext [this])
  (prev! [this node])
  (next! [this node]))

(deftype DLNode [val ^:unsynchronized-mutable prev ^:unsynchronized-mutable next]
  IDLNode
  (gnval [_] val)
  (gprev [_] prev)
  (gnext [_] next)
  (prev! [this node] (set! prev node) this)
  (next! [this node] (set! next node) this))

(defprotocol IDLList
  (ghead [this])
  (gtail [this])
  (head! [this node])
  (tail! [this node]))

(deftype DLList [^:unsynchronized-mutable head ^:unsynchronized-mutable tail]
  IDLList
  (ghead [_] head)
  (gtail [_] tail)
  (head! [this node] (set! head node) this)
  (tail! [this node] (set! tail node) this))

(def dlhead (DLNode. :head nil nil))
(def dltail (DLNode. :tail nil nil))
(next! dlhead dltail)
(prev! dlhead dltail)
(gnval dlhead)
(def dllist (DLList. dlhead dltail))
(gnval (gtail dllist))

(defprotocol ILRUCache
  (lget [this key])
  (lput [this key val]))

;; Not implemented correctly, just a sketch
(defn- accessed [access node]
  (if-let [most-recent (gtail access)]
    (if (= (gnval most-recent) (gnval node))
      access
      (tail! access node))
    (DLList nil node)))

(defn- evict [table access])

(defrecord HLLCache [cap table access]
  ILRUCache
  (lget [this key]
    (if-let [[val node] (get table key)]
      [val (HLLCache. cap table (accessed access node))]
      [nil this]))
  (lput [_ key val]
    (if-let [[_ node] (get table key)]
      (HLLCache. cap (assoc table key [val node]) (accessed access node))
      (let [node    (DLNode. val nil nil)
            updated (assoc table key [val node])]
        (if (< cap (count updated))
          (HLLCache. cap (evict updated access) (accessed access node))
          (HLLCache. cap updated (accessed access node)))))))
