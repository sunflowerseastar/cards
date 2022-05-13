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
(defonce outcomes-default {:win 0 :lose 0 :push 0})
(defonce outcomes (local-storage (atom outcomes-default) :outcomes))

(defonce game (local-storage (atom game-initial-state) :game))

;; the shoe receives (gss @options/num-decks-in-shoe) from deal!
(defonce shoe (atom '[]))
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
(defonce draw-counter (local-storage (atom 4) :draw-counter))
(defonce shoe-counter (local-storage (atom 0) :shoe-counter))


;; ----------------
;; commands/actions
;; ----------------


(defn deal-shoe-hands!
  []
  (if
   ;; no shoe or shoe is exhausted - shuffle fresh shoe and deal from the top
   (or (empty? @shoe) (>= @shoe-counter (count @shoe)))
    (do (println "1 - no shoe or empty shoe, create new shoe and deal and bump counter")
        (reset! shoe (gss @options/num-decks-in-shoe))
        (reset! hands {:you [[(first @shoe) (nth @shoe 2)]]
                       :dealer [(second @shoe) (nth @shoe 3)]})
        (reset! shoe-counter 4))

    ;; shoe exists
    (let [num-cards-remaining-in-shoe (- (count @shoe) @shoe-counter)]
      (do
        (println "2 - cards exist in shoe...: " num-cards-remaining-in-shoe)
        (if
         ;; okay to deal normally...
         (>= num-cards-remaining-in-shoe 4)
          (do
            (println "2a - there are 4 or more cards left, deal those out")
            (reset! hands {:you [[(nth @shoe @shoe-counter) (nth @shoe (+ @shoe-counter 2))]]
                           :dealer [(nth @shoe (inc @shoe-counter)) (nth @shoe (+ @shoe-counter 3))]})
            (swap! shoe-counter #(+ % 4))
            (when (>= @shoe-counter (count @shoe))
              (do
                (println "there were exactly 4 cards left, so it is time to regenerate the shoe (now that those are dealt and shoe is empty)")
                (reset! shoe (gss @options/num-decks-in-shoe))
                (reset! shoe-counter 0))))

          ;; ...otherwise, deal the remaining shoe + the first card(s) of a new, fresh shoe
          (let [remaining-cards (take-last num-cards-remaining-in-shoe @shoe)
                num-cards-needed-from-fresh-shoe (- 4 num-cards-remaining-in-shoe)
                ;; TODO shuffle existing shoe rather than re-generating
                fresh-shoe (gss @options/num-decks-in-shoe)
                cards-from-fresh-shoe (take num-cards-needed-from-fresh-shoe fresh-shoe)
                cards-to-deal (vec (concat remaining-cards cards-from-fresh-shoe))
                [your-first-card dealer-first-card your-second-card dealer-second-card] cards-to-deal]
            (do
              (println "2b - there are 3 or fewer cards")
              (reset! shoe fresh-shoe)
              (reset! hands {:you [[your-first-card your-second-card]]
                             :dealer [dealer-first-card dealer-second-card]})
              (reset! shoe-counter num-cards-needed-from-fresh-shoe))))))))

(defn reset-shoe-and-hands! []
  (do (reset! shoe [])
      (reset! hands {})
      (reset! shoe-counter 0)
      (reset! game game-initial-state)))

(defn reset-win-lose-push! []
  (reset! outcomes outcomes-default))

(defn draw-card! []
  (let [drawn-card (@shoe @shoe-counter)]
    (do
      (swap! shoe-counter inc)
      (when (>= @shoe-counter (count @shoe))
        (do
          (println "reached end, regenerate shoe")
          (reset! shoe (gss @options/num-decks-in-shoe))
          (reset! shoe-counter 0)))
      drawn-card)))

(defn conclude-game! []
  (do
    ;; update scoring
    (let [you-have-split-hands (->> (:you @hands) count (> 1))]
      (doseq [outcome (map #(hands->win-lose-push % (:dealer @hands))
                           (:you @hands))]
        (swap! outcomes update outcome inc)))
    ;; end gameplay
    (swap! game assoc :state :stopped :turn :none)))

(defn add-draw-card-to-hand!
  "Draw a card and add it to the hand currently being played."
  []
  (let [player (:turn @game)
        hit-card (draw-card!)]
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
              (add-draw-card-to-hand!)
              (conclude-game!)))))))

;; Interactive commands

(defn deal! []
  (do
    ;; (reset! deck (generate-shuffled-deck))
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "2 d") (rs "a c")])) ;; deal split aces
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "8 s") (rs "2 d") (rs "8 c")])) ;; deal split 8s
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "k c") (rs "k h")])) ;; you and dealer both have blackjacks
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "9 c") (rs "k h")])) ;; dealer has blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "a s") (rs "a d") (rs "k c") (rs "8 h")])) ;; you have blackjack
    ;; (reset! deck (cards.deck/generate-specific-deck [(rs "2 s") (rs "k d") (rs "2 c") (rs "q d") (rs "2 h") (rs "2 d") (rs "3 s") (rs "3 c") (rs "3 h") (rs "3 d")])) ;; low cards

    ;; beginning of shoe deal
    (deal-shoe-hands!)

    (swap! game assoc :state :running :turn :you :current-split 0)
    (if (= (hand->value (:dealer @hands)) 21)
      ;; if the dealer has blackjack, then there is no interactive gameplay for the round
      (conclude-game!)
      ;; otherwise, if you have blackjack, go ahead and turn gameplay over to the dealer
      (when (= (->> (:you @hands) first hand->value) 21) (dealer-plays!)))))

(defn play-next-split! []
  (swap! game assoc :current-split (inc (:current-split @game))))

(defn draw-and-advance-action!
  "Draw a card, and then proceed to next split or dealer action if hit reach 21 or
  bust. This is usually a hit, but is not technically a hit in the scenario
  where you've split and you're receiving a draw card (+ 'advance-action') for
  each of the split hands."
  []
  (do (add-draw-card-to-hand!)
      (let [your-value (hand->value (nth (:you @hands) (:current-split @game)))
            are-more-splits-remaining (and (= (:turn @game) :you) (> (- (count (:you @hands)) 1) (:current-split @game)))]
        (cond (and are-more-splits-remaining (>= your-value 21)) (do (play-next-split!) (draw-and-advance-action!))
              (>= your-value 21) (dealer-plays!)))))

(defn split! []
  (do (let
          ;; get the second card of the current split hand
       [split-card (nth (nth (:you @hands) (:current-split @game)) 1)]
        ;; ... and create a new hand of just that card
        (swap! hands update :you conj [split-card]))
      ;; ... and then remove that split card from the current split hand
      (swap! hands update-in [:you (:current-split @game)] #(-> (take 1 %) vec))
      (draw-and-advance-action!)))

(defn stand! []
  (if (> (- (count (:you @hands)) 1) (:current-split @game))
    ;; if you have more split hands to play...
    (do (play-next-split!)
        (draw-and-advance-action!))
    ;; ...otherwise dealer's turn
    (dealer-plays!)))

(defn toggle-modal! []
  (swap! game assoc :is-modal-showing (not (:is-modal-showing @game))))
