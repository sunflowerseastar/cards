(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [hand->value hands->win-lose-push]]
   [cards.components :refer [game-state-component hand-component]]
   [cards.deck :refer [generate-shuffled-deck]]
   [reagent.core :as reagent :refer [atom]]))


;; dealer hits everything below, stands on everything equal and above


(def dealer-hit-cutoff 17)


;; Repo convention: state variables get the "normal" name, and components get
;; the cumbersome long name. Ex. (hand-component hand) is the `hand-component`
;; function receiving a piece of game state called `hand`.


(def game-initial-state {;; gameplay
                         :state :stopped ;; stopped | running
                         :turn :none ;; none | you | dealer
                         :current-split 0 ;; hand index of current split

                         ;; outcomes/scoring
                         :wins 0 ;; your total wins
                         :losses 0 ;; your total losses
                         :pushes 0 ;; number of ties

                         ;; ui
                         :is-modal-showing false})

(def game (atom game-initial-state))
(def deck (atom (generate-shuffled-deck)))

;; hands ex. {:you [[{:suit diamond :rank 7} {:suit diamond, :rank 14}]],
;;            :dealer [{:suit heart, :rank 2} {:suit heart, :rank 8}]}
;;
;; Notice that :you get a vector of hands (since you can split), whereas the
;; dealer always only has one hand.
;;
;; For most of the game, :you will have a vector of hands containing just that
;; that one hand. But when you split, there will be additional hands, and
;; :current-split will inc past 0, through each of the split hands.

(def hands (atom {}))
;; TODO change how cards are dealt (draw-counter)
(def draw-counter (atom 4))

(defn deal-hands [local-deck]
  (let [your-card-1 (first local-deck)
        dealer-card-1 (second local-deck)
        your-card-2 (local-deck 2)
        dealer-card-2 (local-deck 3)]
    {:you [[your-card-1 your-card-2]]
     :dealer [dealer-card-1 dealer-card-2]}))

(defn reset-game! []
  (do (reset! deck generate-shuffled-deck)
      (reset! hands {})
      (reset! draw-counter 4)
      (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do (swap! draw-counter inc)
      (@deck @draw-counter)))

(defn end-game! [])

(defn play-next-split! []
  (swap! game assoc :current-split (inc (:current-split @game))))

(defn conclude-game! []
  (do
    ;; update scoring
    (doseq [outcome (map #(hands->win-lose-push % (:dealer @hands)) (:you @hands))]
      (case outcome
        :win (swap! game update :wins inc)
        :lose (swap! game update :losses inc)
        :push (swap! game update :pushes inc)))
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
            (if (< (hand->value (:dealer @hands)) dealer-hit-cutoff)
              (add-hit-card-to-hand!)
              (conclude-game!)))))))

;; Interactive commands

(defn deal! []
  (do
    (reset! deck (generate-shuffled-deck))
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 2} {:suit 'club :rank 14}])) ;; deal a split
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


;; Top-level gameplay markup


(defn blackjack []
  [:div.blackjack-container.padding-lr-sm

   [:div.blocker {:class (if (:is-modal-showing @game) "is-modal-showing")}]
   [:div.modal {:class (if (:is-modal-showing @game) "is-modal-showing")
                :on-click #(toggle-modal!)}
    (game-state-component @game reset-game!)]

   [:div.header [:a {:on-click #(toggle-modal!)} [:div.double-spade]]]

   [:div.game-play-area
    [:div.card-play-area
     (when (not (empty? @hands))
       [:<>

        (let [hand (:dealer @hands)
              is-active false
              is-a-card-in-the-hole (= (:turn @game) :you)]
          (hand-component hand
                          :is-active is-active
                          :is-a-card-in-the-hole is-a-card-in-the-hole))

        [:div.player-division-line
         [:h2 "--- dealer stands on soft 17 ---"]]

        (into [:<>]
              (->> (@hands :you)
                   (map-indexed
                    (fn [i hand]
                      (let [is-active (and (= (:turn @game) :you)
                                           (= (:current-split @game) i))
                            hand-outcome (and (= (:state @game) :stopped)
                                              (hands->win-lose-push hand (:dealer @hands)))
                            is-win (and (= (:state @game) :stopped)
                                        (= hand-outcome :win))]
                        (hand-component hand
                                        :is-active is-active
                                        :hand-outcome hand-outcome))))))])]

    [:div.button-group {:class (when (not= (:state @game) :stopped) "inactive")} [:button {:on-click #(deal!)} "deal"]]

    (let [is-active (and (= (:turn @game) :you) (= (:state @game) :running))
          your-current-hand (nth (:you @hands) (:current-split @game))
          [card-1 card-2 & hits] your-current-hand
          can-stand (some? card-2)
          can-split (and (= (:rank card-1) (:rank card-2)) (empty? hits))
          cannot-hit (and
                      ;; player can't keep hitting if they're playing split hands...
                      (-> (:you @hands) count (> 1))
                      ;; ...and it is aces that were split
                      (= (-> your-current-hand first :rank) 14)
                      ;; ...and they've already hit once.
                      (= (count your-current-hand) 2))]
      [:div.button-group {:class (if (not is-active) "inactive")}
       [:button {:class (if cannot-hit "inactive") :on-click #(hit!)} "hit"]
       [:button {:class (if (not can-stand) "inactive") :on-click #(stand!)} "stand"]
       [:button {:class (if (not can-split) "inactive") :on-click #(split!)} "split"]])]])
