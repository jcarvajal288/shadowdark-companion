(ns shadowdark-companion.dice)

(defn roll-die [n] (inc (rand-int n)))

(defn d4 [] (roll-die 4))
(defn d6 [] (roll-die 6))
(defn d8 [] (roll-die 8))
(defn d10 [] (roll-die 10))
(defn d12 [] (roll-die 12))
(defn d20 [] (roll-die 20))

(defn roll-dice [num size]
  (map (fn [_] (roll-die size)) (range num)))

(defn sum-dice [num size]
  (reduce + (roll-dice num size)))

(defn roll-3d6 [] (sum-dice 3 6))
