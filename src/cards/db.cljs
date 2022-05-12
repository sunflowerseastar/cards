(ns cards.db
  (:require
   [alandipert.storage-atom :refer [local-storage]]
   [cards.blackjack-helpers :refer [hand->value hands->win-lose-push rs]]
   [cards.deck :refer [generate-shuffled-deck gss]]
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

(defonce shoe (local-storage (atom (gss @options/num-decks-in-shoe)) :shoe))
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
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "2 d") (rs "a c")])) ;; deal split aces
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "8 s") (rs "2 d") (rs "8 c")])) ;; deal split 8s
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "k c") (rs "k h")])) ;; you and dealer both have blackjacks
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "9 c") (rs "k h")])) ;; dealer has blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "k c") (rs "8 h")])) ;; you have blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "2 s") (rs "k d") (rs "2 c") (rs "q d") (rs "2 h") (rs "2 d") (rs "3 s") (rs "3 c") (rs "3 h") (rs "3 d")])) ;; low cards
    (reset! hands (deal-hands @deck))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :current-split 0)
    (if (= (hand->value (:dealer @hands)) 21)
      ;; if the dealer has blackjack, then there is no interactive gameplay for the round
      (conclude-game!)
      ;; otherwise, if you have blackjack, go ahead and turn gameplay over to the dealer
      (when (= (->> (:you @hands) first hand->value) 21) (dealer-plays!)))))

(defn play-next-split! []
  (swap! game assoc :current-split (inc (:current-split @game))))

(defn hit! []
  (do (add-hit-card-to-hand!)
      (let [your-value (hand->value (nth (:you @hands) (:current-split @game)))
            are-more-splits-remaining (and (= (:turn @game) :you) (> (- (count (:you @hands)) 1) (:current-split @game)))]
        (cond (and are-more-splits-remaining (>= your-value 21)) (do (play-next-split!) (hit!))
              (>= your-value 21) (dealer-plays!)))))

(defn split! []
  (do (let
          ;; get the second card of the current split hand
       [split-card (nth (nth (:you @hands) (:current-split @game)) 1)]
        ;; ... and create a new hand of just that card
        (swap! hands update :you conj [split-card]))
      ;; ... and then remove that split card from the current split hand
      (swap! hands update-in [:you (:current-split @game)] #(-> (take 1 %) vec))
      (hit!)))

(defn stand! []
  (if (> (- (count (:you @hands)) 1) (:current-split @game))
    (do (play-next-split!)
        (hit!))
    (dealer-plays!)))

(defn toggle-modal! []
  (swap! game assoc :is-modal-showing (not (:is-modal-showing @game))))
