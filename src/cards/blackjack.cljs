(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [hand->value]]
   [cards.components :refer [game-status hand-component]]
   [cards.deck :refer [generate-shuffled-deck]]
   [reagent.core :as reagent :refer [atom]]))

(def dealer-hit-cutoff 17)


;; State variables get the "normal" name, and components get the cumbersome long
;; name. Ex. (hand-component hand) is the `hand-component` function receiving a
;; piece of game state called `hand`.


(def game-initial-state {:state :stopped ;; stopped | running
                         :turn :none ;; none | you | dealer
                         :current-split 0 ;; hand index of current split
                         :dealer-wins 0 ;; number of wins for dealer
                         :your-wins 0 ;; number of wins for you
                         :pushes 0 ;; number of pushes
                         :current-winner nil ;; who just won the finished hand
                         :is-modal-showing false
                         :results [] ;; explanation of who won & why
                         })

(def game (atom game-initial-state))
;; TODO actually use the shoe... this is a deck, not a shoe
(def deck (atom (generate-shuffled-deck)))
;; Ex. {:you [{:card-1 {:suit diamond, :rank 7}, :card-2 {:suit diamond, :rank 14}}],
;;      :dealer [{:card-1 {:suit heart, :rank 2}, :card-2 {:suit heart, :rank 8}}]}
;; Dealer gets a vector of hands, although in practice the dealer always has only one hand.
(def hands (atom {}))
(def draw-counter (atom 4))

(defn deal-hands [local-deck]
  (let [your-card-1 (first local-deck)
        dealer-card-1 (second local-deck)
        your-card-2 (local-deck 2)
        dealer-card-2 (local-deck 3)]

    ;; {:you [your-card-1 your-card-2]
    ;;  :dealer [dealer-card-1 dealer-card-2]}

    {:you [{:card-1 your-card-1, :card-2 your-card-2}]
     :dealer [{:card-1 dealer-card-1, :card-2 dealer-card-2}]}

    ))

(defn reset-game! []
  (do
    (reset! deck generate-shuffled-deck)
    (reset! hands {})
    (reset! draw-counter 4)
    (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@deck @draw-counter)))

(defn end-game! []
  (swap! game assoc :state :stopped :turn :none))

(defn increment-wins! [win-count]
  (swap! game assoc win-count (inc (@game win-count))))

(defn you-win! []
  (swap! game assoc :your-wins (inc (:your-wins @game)) :current-winner :you))

(defn dealer-wins! []
  (swap! game assoc :dealer-wins (inc (:dealer-wins @game)) :current-winner :dealer))

(defn push! []
  (increment-wins! :pushes))

(defn update-result! [result-text]
  (swap! game assoc :results (conj (:results @game) result-text)))

(defn conclude [winner result-text]
  (do
    (update-result! result-text)
    (case winner
      :push (push!)
      :dealer (dealer-wins!)
      (you-win!))))

(defn play-next-split! []
  (swap! game assoc :current-split (inc (:current-split @game))))

(defn conclude-game! []
  (let [your-values (map hand->value (:you @hands))
        dealer-value (hand->value (nth (:dealer @hands) 0))]
    (do
      (doseq [your-value your-values]
        (cond (> your-value 21) (conclude :dealer "dealer wins - you bust")
              (> dealer-value 21) (conclude :you "you win - dealer busts")
              (= your-value dealer-value) (conclude :push "push")
              (= your-value 21) (conclude :you "you win - blackjack :)")
              (= dealer-value 21) (conclude :dealer "dealer wins - blackjack")
              (> dealer-value your-value) (conclude :dealer "dealer wins - higher value")
              :else (conclude :you "you win - higher value")))
      (end-game!))))

(defn add-hit-card-to-hand! [player card]
  (if (and (= (:turn @game) :you)
           (nil? ((nth (@hands :you) (:current-split @game)) :card-2)))
    (swap! hands assoc-in [player (:current-split @game) :card-2] card)
    (swap! hands assoc-in [player (:current-split @game) :hits]
           (vec (conj ((nth (@hands player) (:current-split @game)) :hits) card)))))

(defn dealer-plays! []
  (do
    (swap! game assoc :turn :dealer :current-split 0)
    (while (= (:state @game) :running)
      (if (< (hand->value (nth (:dealer @hands) 0)) dealer-hit-cutoff)
        (add-hit-card-to-hand! :dealer (draw-hit-card!))
        (conclude-game!)))))

(defn deal! []
  (do
    ;; (reset! deck (generate-shuffled-deck))
    (reset! deck (cards.deck/generate-specific-deck [{:suit 'spade :rank 14} {:suit 'diamond :rank 2} {:suit 'club :rank 14}]))
    (reset! hands (deal-hands @deck))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :current-split 0 :current-winner nil :results [])
    (if (= (hand->value (nth (:you @hands) (:current-split @game))) 21) (dealer-plays!))))

(defn stand! []
  (if (> (- (count (:you @hands)) 1) (:current-split @game))
    (play-next-split!)
    (dealer-plays!)))

(defn split! []
  (do
    (swap! hands assoc-in [:you (+ (:current-split @game) 1) :card-1] ((nth (:you @hands) (:current-split @game)) :card-2))
    (swap! hands update-in [:you (:current-split @game)] dissoc :card-2)))

(defn hit! []
  (do
    (add-hit-card-to-hand! :you (draw-hit-card!))
    (let [your-value (hand->value (nth (:you @hands) (:current-split @game)))
          more-splits-remaining-p (and (= (:turn @game) :you) (> (- (count (:you @hands)) 1) (:current-split @game)))]
      (cond (and more-splits-remaining-p (>= your-value 21)) (play-next-split!)
            (>= your-value 21) (dealer-plays!)))))

(defn blackjack []
  [:div.main.blackjack-container {:class (if (:is-modal-showing @game) "is-modal-showing")}

   [:div.stats {:class (if (:is-modal-showing @game) "active")
                :on-click #(swap! game assoc :is-modal-showing (not (:is-modal-showing @game)))}
    [game-status ^{:class "stats"} @game]]
   [:a.modal-a {:on-click #(swap! game assoc :is-modal-showing (not (:is-modal-showing @game)))}]

   [:div.button-group
    [:button {:on-click #(deal!)} "deal"]
    [:button {:on-click #(reset-game!)} "reset"]]

   [:div.card-play-area
    (when (not (empty? @hands))
      [:<>

       [:div.player-container.dealer
        [:h2 {:class (if (= (:current-winner @game) :dealer) "win")} "dealer"]
        [:div.hands
         (let [hand (first (:dealer @hands))
               value (hand->value hand)
               is-active false]
           (when hand (hand-component hand is-active)))]]

       [:div.player-container.you
        [:h2 {:class (if (= (:current-winner @game) :you) "win")} "you"]
        (into [:div.hands]
              (->> (@hands :you)
                   (map-indexed
                    (fn [i hand]
                      (let [is-active (and (= (:turn @game) :you)
                                           (or (= (count (:you @hands)) 1)
                                               (= (:current-split @game) i)))]
                        (hand-component hand is-active))))))]])]

   (let [active-p (and (= (:turn @game) :you) (= (:state @game) :running))
         {:keys [card-1 card-2 hits], :or {card-1 {} card-2 {} hits []}}
         (nth (@hands :you) (:current-split @game))
         can-stand-p (not-empty card-2)
         can-split-p (and (= (:rank card-1) (:rank card-2)) (empty? hits))]
     [:div.button-group {:class (if (not active-p) "inactive")}
      [:button {:on-click #(hit!)} "hit"]
      [:button {:class (if (not can-stand-p) "inactive") :on-click #(stand!)} "stand"]
      [:button {:class (if (not can-split-p) "inactive") :on-click #(split!)} "split"]])])
