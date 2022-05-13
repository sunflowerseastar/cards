(ns cards.deck
  (:require
   [cards.options :as options]))

;; chunk: a contiguous group of cards in a deck or shoe.

(defn generate-sorted-deck
  "Pulling from the top (A is low):
     - A -> K hearts
     - A -> K clubs
     - K -> A diamond
     - K -> A spade"
  []
  (let [A->K-ranks [14 2 3 4 5 6 7 8 9 10 11 12 13]
        K->A-ranks (reverse A->K-ranks)
        A->K-hearts-and-clubs (for [suit [:heart :club] rank A->K-ranks] {:suit suit :rank rank})
        K->A-diamonds-and-spades (for [suit [:diamond :spade] rank K->A-ranks] {:suit suit :rank rank})]
    (vec (concat A->K-hearts-and-clubs K->A-diamonds-and-spades))))

(def deck (atom (generate-sorted-deck)))

;; deck helpers

(defn plus-minus
  "Given an int and a precision, return a random int that's within plus/minus
  (* precision n) of n. Ex. n of 100 with 90% precision returns rand-int range
  90 to 110 -- a 10% error of 100 is 10, and that 10 can manifest from 10 below n
  to 10 above n."
  [n precision]
  (let [imprecision (->> n (* precision) (- n) (* 2) inc)] ; inc is because rand-int plus is exclusive
    (+ n (- (rand-int imprecision) (if (zero? imprecision) 0 (quot imprecision 2))))))

(defn split-at-nth-with-precision
  "Split the cards at a certain nth point, adjusted plus/minus per precision. For
  most of deck.cljs, I avoid the word 'split' and stick to divide, but this
  function talks about splits since it's using clojure.core's split-at. Just
  note that these splits have nothing to with split hands, or splitting a hand
  in gameplay. For those, see :current-split, split!, and related in db.cljs."
  ([deck-or-shoe nth] (split-at-nth-with-precision deck-or-shoe nth @options/shuffle-precision))
  ([deck-or-shoe nth precision]
   (let [num-cards (count deck-or-shoe)
         split-point (plus-minus nth precision)]
     (split-at split-point deck-or-shoe))))

(defn divide-cards
  "Given a deck or shoe, divide it in two and return the halves in a vector."
  ([deck-or-shoe] (divide-cards deck-or-shoe @options/shuffle-precision))
  ([deck-or-shoe precision]
   (let [half-point (quot (count deck-or-shoe) 2)]
     (split-at-nth-with-precision deck-or-shoe half-point precision))))

;; deck actions

(defn cut
  "A 'regular' cut: divide the cards in two (plus-minus precision), and stack the
  previously lower portion on top. See note about cut precision in
  core_test.cljs."
  ([deck-or-shoe]
   (cut deck-or-shoe @options/shuffle-precision))
  ([deck-or-shoe precision]
   (let [[top bottom] (divide-cards deck-or-shoe precision)]
     (vec (concat bottom top)))))

(defn cut-one-third-top
  "Take top 1/3 cards (adjusted for precision), and place them on bottom."
  ([deck-or-shoe] (cut-one-third-top deck-or-shoe @options/shuffle-precision))
  ([deck-or-shoe precision]
   (let [top-third-point (quot (count deck-or-shoe) 3)
         [top bottom] (split-at-nth-with-precision deck-or-shoe top-third-point precision)] (vec (concat bottom top)))))

;; Just an alias. The physical process of boxing versus bottom 1/3 cut are
;; different, but they are the same, card programming-wise.
(def box cut-one-third-top)

(defn cut-one-third-bottom
  "Take bottom 1/3 cards (adjusted for precision), and place them on top."
  ([deck-or-shoe] (cut-one-third-top deck-or-shoe @options/shuffle-precision))
  ([deck-or-shoe precision]
   (let [bottom-third-point (-> deck-or-shoe count (quot 3) (* 2))
         [top bottom] (split-at-nth-with-precision deck-or-shoe bottom-third-point precision)] (vec (concat bottom top)))))

(defn riffle-lr
  "Given two halves of a deck, imprecisely zipper them together. A card is
  selected from either side in alternation, except for 'errors' when (rand)
  doesn't reach preicision, in which a card from the previous side is repeated.
  The intuition regarding left/right is that the card chunks are in the left &
  right hands."
  ([l-r-chunks] (riffle-lr l-r-chunks @options/shuffle-precision))
  ([[left right] precision]
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
         :else (recur l (rest r) (conj shuffled-deck (first r)) next-is-card-l))))))

(defn riffle
  "Helper function to riffle a full deck. The riffle-lr function
  is separate because it shuffles l/r chunks in a shoe."
  ([deck] (riffle deck @options/shuffle-precision))
  ([deck precision]
   (riffle-lr (divide-cards deck precision) precision)))

(defn strip
  "Given a deck, keep taking 1/x of the top cards (3 to 7 times), and stacking
  them (new top-cards are placed on top of the growing shuffled-deck), until all
  cards have been placed on the shuffled-deck."
  ([deck] (strip deck @options/shuffle-precision))
  ([deck precision]
   (let [rough-chunk-size (quot 52 (plus-minus 5 precision))]
     (loop [cards-to-take (plus-minus rough-chunk-size precision)
            top-chunk (take cards-to-take deck)
            remaining-deck (drop cards-to-take deck)
            shuffled-deck '()]
       (if (empty? remaining-deck) (concat top-chunk shuffled-deck)
           (recur (plus-minus rough-chunk-size precision)
                  (take cards-to-take remaining-deck)
                  (drop cards-to-take remaining-deck)
                  (concat top-chunk shuffled-deck)))))))

;; generators (as in, create a deck or shoe)

(defn generate-shoe
  "Return n sorted decks, combined."
  [n]
  (->> (repeatedly #(generate-sorted-deck)) (take n) (apply concat)))

(defn generate-shuffled-shoe
  "Return a shuffled shoe comprised of n decks. Roughly based on
  https://www.youtube.com/watch?v=tpv5sqoveuc. 'Stack' refers to the two
  original divided shoe stacks that sit on the left and right. 'Chunk' refers to
  the cards that are either in the left hand or right hand that will be shuffled
  together in a given iteration."
  [n]
  (let [shoe (generate-shoe n)
        [left right] (divide-cards shoe)
        ;; TODO make ctt (cards to take) vary, based on precision
        ctt 26]
    (println (count shoe) (count left) (count right))
    (loop [left-stack left
           right-stack right
           working-deck '()
           is-working-left false]

      (do
        ;; (println "--")
        ;; (println (count left-stack) (count right-stack))
        ;; (println (count working-deck))
        (if
         (and (empty? left-stack) (empty? right-stack)) working-deck ; conclusion

         (let [use-working-deck (not (empty? working-deck))
               left-chunk (vec (take ctt (if (and use-working-deck is-working-left) working-deck left-stack)))
               left-stack-remaining (if (and use-working-deck is-working-left) left-stack (drop ctt left-stack))
               right-chunk (vec (take ctt (if (and use-working-deck (not is-working-left)) working-deck right-stack)))
               right-stack-remaining (if (and use-working-deck (not is-working-left)) right-stack (drop ctt right-stack))
               shuffled-chunks (->> (riffle-lr [left-chunk right-chunk])
                                    strip
                                    riffle)]
           (do
             ;; (println "lsr " (count left-stack-remaining) " | rsr " (count right-stack-remaining))
             ;; (println "sc " (count shuffled-chunks))
             (recur
              left-stack-remaining
              right-stack-remaining
              ;; add the newly shuffled cards on top of the working-deck
              (vec (concat shuffled-chunks (drop ctt working-deck)))
              ;; alternate which hand will pull from the working deck
              (not is-working-left)))))))))

(defn generate-shuffled-deck
  "Return a deck that is shuffled."
  []
  (->> (generate-sorted-deck)
       strip
       riffle
       strip
       riffle
       riffle
       cut
       vec))

(defn generate-specific-deck [starting-cards]
  (->> (generate-shuffled-deck)
       ;; remove starting-cards from the deck
       (filter (fn [card] (not (some #(= card %) starting-cards))))
       ;; put starting-cards at the front of the deck
       (concat starting-cards)
       vec))
