(ns fsm)

(def fsm1 {:s1 :s2
           :s2 :s1})

(defn make-transition [s1 s2]
  {s1 s2})

(defn make-fsm [transitions]
  (apply merge transitions))

(defn state [fsm-state]
  (second fsm-state))

(defn transition [[fsm state]]
  [fsm (fsm state)])

(def fsm2
  (make-fsm [(make-transition :s1 :s2)
             (make-transition :s2 :s3)
             (make-transition :s3 :s1)]))

(def fsm1s1 [fsm1 :s1])
(def fsm2s1 [fsm2 :s1])

(mapv second (take 9 (iterate transition fsm2s1)))

(defn make-transition-key [s1 key s2]
  {[s1 key] s2})

(defn transition-key [[fsm state] key]
  (if-let [next-state (fsm [state key])]
    [fsm next-state]
    (if-let [wild-state (fsm [state \*])]
      [fsm wild-state]
      [fsm state])))

(defn transition-keys [fsm-state keys]
  (reduce (fn [cur-fsm-state key] (transition-key cur-fsm-state key))
          fsm-state
          keys))

(def ex1
  (make-fsm [(make-transition-key :a \a :b)
             (make-transition-key :a \* :err)
             (make-transition-key :b \b :b)
             (make-transition-key :b \c :b)
             (make-transition-key :b \d :d)
             (make-transition-key :b \* :err)]))
(transition-key [ex1 :a] \a)
(ex1 [:a \a])
(state (transition-keys [ex1 :a] "ae"))
