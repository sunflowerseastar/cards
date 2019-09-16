(ns cards.blackjack-helpers)

(defn cards-from-hand [hand]
  (let [{:keys [card-1 card-2 hits]} hand]
    (vec (filter not-empty (flatten (conj [card-1] [card-2] hits))))))

(defn translate-rank-to-value [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 11))

(defn value [hand]
  (reduce + (map #(translate-rank-to-value (% :rank)) (cards-from-hand hand))))
