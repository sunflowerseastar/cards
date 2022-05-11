(ns cards.db
  (:require
   [alandipert.storage-atom :refer [local-storage]]
   [cards.blackjack-helpers :refer [hand->value hands->win-lose-push]]
   [cards.deck :refer [generate-shuffled-deck]]
   [cards.options :as options]
   [reagent.core :refer [atom]]))

;; -------------
;; consts, state
;; -------------

;; Repo convention: state variables get the "normal" name, and components get
;; the cumbersome long name. Ex. (hand-component hand) is the `hand-component`
;; function receiving a piece of game state called `hand`.

(defonce game-initial-state {;; gameplay
                             :state :stopped ;; stopped | running
                             :turn :none ;; none | you | dealer
                             :current-split 0 ;; hand index of current split

                             ;; ui
                             :is-modal-showing false})

;; a scoreboard that increments for each hand's win, loss, or push
(defonce outcomes (local-storage (atom {:win 0 :lose 0 :push 0}) :outcomes))

(defonce game (local-storage (atom game-initial-state) :game))
(defonce deck (local-storage (atom (generate-shuffled-deck)) :deck))

;; hands ex. {:you [[{:suit diamond :rank 7} {:suit diamond, :rank 14}]],
;;            :dealer [{:suit heart, :rank 2} {:suit heart, :rank 8}]}
;;
;; Notice that :you get a vector of hands (since you can split), whereas the
;; dealer always only has one hand.
;;
;; For most of the game, :you will have a vector of hands containing just that
;; that one hand. But when you split, there will be additional hands, and
;; :current-split will inc past 0, through each of the split hands.

(defonce hands (local-storage (atom {}) :hands))
;; TODO change how cards are dealt (draw-counter)
(defonce draw-counter (local-storage (atom 4) :draw-counter))


;; ----------------
;; commands/actions
;; ----------------


(defn deal-hands [local-deck]
  (let [your-card-1 (first local-deck)
        dealer-card-1 (second local-deck)
        your-card-2 (local-deck 2)
        dealer-card-2 (local-deck 3)]
    {:you [[your-card-1 your-card-2]]
     :dealer [dealer-card-1 dealer-card-2]}))

(defn reset-game! []
  (do (reset! deck (generate-shuffled-deck))
      (reset! hands {})
      (reset! draw-counter 4)
      (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do (swap! draw-counter inc)
      (@deck @draw-counter)))

(defn end-game! [])

;; TODO verify that split cards are dealt correctly
(defn play-next-split! []
  (swap! game assoc :current-split (inc (:current-split @game))))

(defn conclude-game! []
  (do
    ;; update scoring
    (doseq [outcome (map #(hands->win-lose-push % (:dealer @hands)) (:you @hands))]
      (swap! outcomes update outcome inc))
    ;; end gameplay
    (swap! game assoc :state :stopped :turn :none)))

(defn add-hit-card-to-hand!
  "Draw a card and add it to the hand currently being played."
  []
  (let [player (:turn @game)
        hit-card (draw-hit-card!)]
    (if (= player :you)
      (swap! hands update-in [:you (:current-split @game)] conj hit-card)
      (swap! hands update :dealer conj hit-card))))

(defn dealer-plays! []
  (do (swap! game assoc :turn :dealer)
      (let [you-busted-all-hands
            (->> (:you @hands) (map hand->value) (filter #(<= % 21)) empty?)]
        (if you-busted-all-hands
          (conclude-game!)
          (while (= (:state @game) :running)
            (if (< (hand->value (:dealer @hands)) (if options/dealer-stands-on-17 17 18))
              (add-hit-card-to-hand!)
              (conclude-game!)))))))

;; Interactive commands

(defn deal! []
  (do
    (reset! deck (generate-shuffled-deck))
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 2} {:suit 'club :rank 14}])) ;; deal split aces
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 8} {:suit 'diamond :rank 2} {:suit 'club :rank 8}])) ;; deal split 8s
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 14} {:suit 'club :rank 13} {:suit 'heart :rank 13}])) ;; you and dealer both have blackjacks
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 14} {:suit 'club :rank 9} {:suit 'heart :rank 13}])) ;; dealer has blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 14} {:suit 'club :rank 13} {:suit 'heart :rank 8}])) ;; you have blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 2} {:suit 'diamond :rank 13} {:suit 'club :rank 2} {:suit 'diamond :rank 2} {:suit 'heart :rank 2} {:suit 'heart :rank 3}])) ;; low cards
    (reset! hands (deal-hands @deck))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :current-split 0)
    (if (= (hand->value (:dealer @hands)) 21)
      ;; if the dealer has blackjack, then there is no interactive gameplay for the round
      (conclude-game!)
      ;; otherwise, if you have blackjack, go ahead and turn gameplay over to the dealer
      (when (= (->> (:you @hands) first hand->value) 21) (dealer-plays!)))))

(defn stand! []
  (if (> (- (count (:you @hands)) 1) (:current-split @game))
    (play-next-split!)
    (dealer-plays!)))

(defn split! []
  (do (let [split-card (nth (nth (:you @hands) (:current-split @game)) 1)]
        (swap! hands update :you conj [split-card]))
      (swap! hands update-in [:you (:current-split @game)] #(-> (take 1 %) vec))))

(defn hit! []
  (do (add-hit-card-to-hand!)
      (let [your-value (hand->value (nth (:you @hands) (:current-split @game)))
            are-more-splits-remaining (and (= (:turn @game) :you) (> (- (count (:you @hands)) 1) (:current-split @game)))]
        (cond (and are-more-splits-remaining (>= your-value 21)) (play-next-split!)
              (>= your-value 21) (dealer-plays!)))))

(defn toggle-modal! []
  (swap! game assoc :is-modal-showing (not (:is-modal-showing @game))))
