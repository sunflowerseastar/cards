(ns cards.deck)

(defn generate-deck
  "Pulling from the top (A is low):
     - A -> K hearts
     - A -> K clubs
     - K -> A diamond
     - K -> A spade"
  []
  (let [A->K-ranks [14 2 3 4 5 6 7 8 9 10 11 12 13]
        K->A-ranks (reverse A->K-ranks)
        A->K-hearts-and-clubs (for [suit ['heart 'club] rank A->K-ranks] {:suit suit :rank rank})
        K->A-diamonds-and-spades (for [suit ['diamond 'spade] rank K->A-ranks] {:suit suit :rank rank})]
    (vec (concat A->K-hearts-and-clubs K->A-diamonds-and-spades))))

(def deck (atom (generate-deck)))

(defn translate-rank-of [rank]
  (case rank
    11 'J
    12 'Q
    13 'K
    14 'A
    rank))

(defn shuffle-riffle
  "Given two halves of a deck, imprecisely zipper them together. A card is
  selected from either side in alternation, except for 'errors' when (rand)
  doesn't reach preicision, in which a card from the previous side is repeated."
  [left right]
  (let [precision 0.9]
    (loop [l left r right shuffled-deck [] is-card-l (< (rand) 0.5)]
      (cond (and (empty? l) (empty? r)) shuffled-deck
            (empty? l) (concat shuffled-deck r)
            (empty? r) (concat shuffled-deck l)

            (and (< (rand) precision) is-card-l) (recur (rest l) r (conj shuffled-deck (first l)) false)
            (< (rand) precision) (recur l (rest r) (conj shuffled-deck (first r)) true)

            is-card-l (recur (rest l) r (conj shuffled-deck (first l)) true)
            :else (recur l (rest r) (conj shuffled-deck (first r)) false)))))

(defn divide-deck [deck]
  (let [separate-point (+ (/ (count deck) 2) (- (rand-int 10) 5))]
    [(take separate-point deck) (drop separate-point deck)]))

(defn shuffler [deck shuffle-fn]
  (let [l-r-deck (divide-deck deck)] (shuffle-fn (first l-r-deck) (second l-r-deck))))

(defn shuffle-deck [deck]
  (vec (shuffler deck shuffle-riffle)))

(defn generate-shuffled-deck
  "Return a deck that is shuffled."
  []
  (let [d (generate-deck)] (nth (iterate shuffle d) 6)))

(defn generate-specific-deck [starting-cards]
  (->> (generate-shuffled-deck)
       ;; remove starting-cards from the deck
       (filter (fn [card] (not (some #(= card %) starting-cards))))
       ;; put starting-cards at the front of the deck
       (concat starting-cards)
       vec))
