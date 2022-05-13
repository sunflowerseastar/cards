(ns cards.blackjack-helpers
  (:require
   [clojure.string :refer [lower-case split]]))

(defn rank->hard-value [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 1))

(defn hand->value
  "Determine hand value, which will be soft if possible."
  [hand]
  (let [hard-value (reduce + (map #(rank->hard-value (:rank %)) hand))
        soft-value (when (some #(= (:rank %) 14) hand)
                     (+ hard-value 10))]
    (if (and (pos? soft-value) (<= soft-value 21))
      soft-value
      hard-value)))

(defn hands->win-lose-push
  "Given one of your hands and the dealer hand, return 1 of #{:win :lose :push}.
  'you-have-split-hands' is used to distinguish natural blackjack from a 21 if
  you're playing split hands."
  ([your-hand dealer-hand] (hands->win-lose-push your-hand dealer-hand false))
  ([your-hand dealer-hand you-have-split-hands]

   (let [;; helpers
         is-two-cards #(->> % count (= 2))
         dealer-value (hand->value dealer-hand)
         your-value (hand->value your-hand)

         ;; names for clarity/simplicity
         dealer-blackjack (and (= dealer-value 21) (is-two-cards dealer-hand))
         you-blackjack (and (not you-have-split-hands)
                            (= your-value 21)
                            (is-two-cards your-hand))
         dealer-busts (> dealer-value 21)
         you-bust (> your-value 21)
         push (= your-value dealer-value)
         dealer-is-higher (> dealer-value your-value)
         you-are-higher (> your-value dealer-value)]

     (cond you-bust :lose

           (and dealer-blackjack you-blackjack) :push
           you-blackjack :win
           dealer-blackjack :lose

           dealer-busts :win

           dealer-is-higher :lose
           you-are-higher :win
           push :push

           ;; unreachable, TODO instrument
           :else (throw (js/Error. "condition not handled ☠"))))))

(defn rs
  "Take a 'rank suit' (aka 'rs') card notation shorthand and return the regular
  card data structure."
  [str]
  (let [[raw-rank raw-suit] (split str #" ")
        raw-rank-lower (lower-case raw-rank)
        rank (cond
               (= raw-rank-lower \a) 14
               (= raw-rank-lower \k) 13
               (= raw-rank-lower \q) 12
               (= raw-rank-lower \j) 11
               :else (js/parseInt raw-rank))
        suit (cond
               (= raw-suit \s) :spade
               (= raw-suit \c) :club
               (= raw-suit \d) :diamond
               (= raw-suit \h) :heart)]
    {:suit suit :rank rank}))
