(ns cards.blackjack-helpers
  (:require [tupelo.core :refer [spyx]]))

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
  "Given one of your hands and the dealer hand, return 1 of #{:win :lose :push}."
  [your-hand dealer-hand]

  (let [;; helpers
        is-two-cards #(->> % count (= 2))
        dealer-value (hand->value dealer-hand)
        your-value (hand->value your-hand)

        ;; names for clarity/simplicity
        ;; TODO don't consider A-[10] cards a blackjack for split hands
        dealer-blackjack (and (= dealer-value 21) (is-two-cards dealer-hand))
        you-blackjack (and (= your-value 21) (is-two-cards your-hand))
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
          :else (throw (js/Error. "condition not handled ☠")))))
