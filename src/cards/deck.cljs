(ns cards.deck)

(defn generate-deck []
  (vec (for [suit ['spade 'club 'diamond 'heart] rank [2 3 4 5 6 7 8 9 10 11 12 13 14]] {:suit suit :rank rank})))

(def deck (atom (generate-deck)))

(defn translate-rank-of [rank]
  (case rank
    11 'J
    12 'Q
    13 'K
    14 'A
    rank))

(defn weighted-shuffle [a b]
  (loop [a a b b l [] probably-a 50]
    (let [r (rand-int 100)]
      (cond (and (empty? a) (empty? b)) l
            (empty? a) (concat l b)
            (empty? b) (concat l a)
            (< r probably-a) (recur (rest a) b (conj l (first a)) (- probably-a 15))
            :else (recur a (rest b) (conj l (first b)) (+ probably-a 15))))))

(defn divide-deck [deck]
  (let [separate-point (+ (/ (count deck) 2) (- (rand-int 10) 5))]
    [(take separate-point deck) (drop separate-point deck)]))

(defn shuffler [deck shuffle-fn]
  (let [l-r-deck (divide-deck deck)] (shuffle-fn (first l-r-deck) (second l-r-deck))))

(defn shuffle-deck [deck]
  (vec (shuffler deck weighted-shuffle)))

(defn generate-shoe []
  (let [d (generate-deck)] (nth (iterate shuffle d) 6)))

(defn generate-specific-shoe [starting-cards]
  (->> (generate-shoe)
       (filter #(not ((into #{} [{:suit 's :rank 14} {:suit 'c :rank 14}]) %)))
       (conj starting-cards)
       flatten
       vec))
