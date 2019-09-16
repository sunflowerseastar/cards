(ns cards.blackjack-helpers)

(defn cards-from-hand [hand]
  (let [{:keys [card-1 card-2 hits]} hand]
    (vec (filter not-empty (flatten (conj [card-1] [card-2] hits))))))

(defn translate-rank-hard [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 11))

(defn translate-rank-soft [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 1))

(defn has-ace-p [hand]
  (some #(= (% :rank) 14) (cards-from-hand hand)))

(defn choose-value [v1 v2]
  (let [lower-value (if (< v2 v1) v2 v1)
        higher-value (if (< v2 v1) v1 v2)]
    (cond (and (> v1 21) (< v2 21)) v2
          (and (> v2 21) (< v1 21)) v1
          (and (> v1 21) (> v2 21)) lower-value
          (and (< v1 21) (< v2 21)) higher-value
          :else v1)))

(defn value-sum [hand translater]
  (reduce + (map #(translater (% :rank)) (cards-from-hand hand))))

(defn value [hand]
  (if (has-ace-p hand)
    (choose-value (value-sum hand translate-rank-hard)
                  (value-sum hand translate-rank-soft))
    (value-sum hand translate-rank-soft)))
