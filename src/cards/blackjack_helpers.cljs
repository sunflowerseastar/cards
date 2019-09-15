(ns cards.blackjack-helpers)

(defn cards-from-hand [hand]
  (let [{:keys [card-1 card-2 hits]} hand]
    (vec (filter not-empty (flatten (conj [card-1] [card-2] hits))))))

(defn sum [player hands]
  (reduce + (map :rank (cards-from-hand (hands player)))))
