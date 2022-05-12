(ns cards.deck
  (:require
   [cards.options :as options]))

(defn sorted-deck
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

(def deck (atom (sorted-deck)))

(defn translate-rank-of [rank]
  (case rank
    11 'J
    12 'Q
    13 'K
    14 'A
    rank))

(defn num-adjusted-for-precision
  "Given an int and a precision, return a random int that within plus/minus
  (* precision n) of n."
  [n precision]
  (let [imprecision (- n (* precision n))]
    (+ n (- (rand-int imprecision) (if (zero? imprecision) 0 (quot imprecision 2))))))

(defn divide-deck
  ;; TODO replace with split?
  "Given a deck, split it in two and return the halves in a vector."
  ([deck] (divide-deck deck @options/shuffle-precision))
  ([deck precision]
   (let [num-cards (count deck)
         num-half (quot num-cards 2)
         imprecision (- num-cards (* precision num-cards))
         separate-point (num-adjusted-for-precision num-half precision)]
     [(take separate-point deck) (drop separate-point deck)])))

(defn riffle-shuffle
  "Given two halves of a deck, imprecisely zipper them together. A card is
  selected from either side in alternation, except for 'errors' when (rand)
  doesn't reach preicision, in which a card from the previous side is repeated."
  ([deck] (riffle-shuffle deck @options/shuffle-precision))
  ([deck precision]
   ;; the intuition is that divided decks are in the left & right hands
   (let [[left right] (divide-deck deck)]
     ;; 'is-card-l' means 'the bottom card of the left deck is going to go on top of the shuffled-deck
     (loop [l (reverse left) r (reverse right) shuffled-deck '() is-card-l (< (rand) precision)]
       (let [;; this determines whether the shuffles alternates correctly,
             ;; or if there's an "error," and the next iteration will place its
             ;; card from the same side again.
             next-is-card-l (if (< (rand) precision) (not is-card-l) is-card-l)]
         (cond
            ;; the first three cases are finishing a shuffle
           (and (empty? l) (empty? r)) shuffled-deck
           (empty? l) (apply conj shuffled-deck r)
           (empty? r) (apply conj shuffled-deck l)

           ;; the last two cases are placing either the left card or right card & recurring
           is-card-l (recur (rest l) r (conj shuffled-deck (first l)) next-is-card-l)
           :else (recur l (rest r) (conj shuffled-deck (first r)) next-is-card-l)))))))

(defn strip-shuffle
  "Given a deck, keep taking 1/x of the top cards (3 to 7 times), and stacking
  them (new top-cards are placed on top of the growing shuffled-deck), until all
  cards have been placed on the shuffled-deck."
  ([deck] (strip-shuffle deck @options/shuffle-precision))
  ([deck precision]
   (let [strip-precision (* 0.8 precision) ; otherwise very little variation
         rough-chunk-size (quot 52 (num-adjusted-for-precision 5 strip-precision))]
     (loop [cards-to-take (num-adjusted-for-precision rough-chunk-size strip-precision)
            top-chunk (take cards-to-take deck)
            remaining-deck (drop cards-to-take deck)
            shuffled-deck '()]
       (if (empty? remaining-deck) (apply conj shuffled-deck top-chunk)
           (recur (num-adjusted-for-precision rough-chunk-size strip-precision)
                  (take cards-to-take remaining-deck)
                  (drop cards-to-take remaining-deck)
                  (apply conj shuffled-deck top-chunk)))))))

(defn cut-deck
  "Split a deck and stack the previously lower portion on top."
  [deck]
  (let [[top bottom] (divide-deck deck)] (vec (concat bottom top))))

(defn shuffler [deck shuffle-fn]
  (let [[top bottom] (divide-deck deck)] (shuffle-fn top bottom)))

(defn generate-shuffled-deck
  "Return a deck that is shuffled."
  []
  (->> (sorted-deck)
       strip-shuffle
       riffle-shuffle
       strip-shuffle
       riffle-shuffle
       riffle-shuffle
       cut-deck
       vec))

(defn generate-specific-deck [starting-cards]
  (->> (generate-shuffled-deck)
       ;; remove starting-cards from the deck
       (filter (fn [card] (not (some #(= card %) starting-cards))))
       ;; put starting-cards at the front of the deck
       (concat starting-cards)
       vec))
