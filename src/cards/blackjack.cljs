(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [value]]
   [cards.components :refer [card-hand card-row]]
   [cards.deck :refer [generate-shoe]]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(def dealer-hit-cutoff 17)

(def game-initial-state {:state :stopped
                         :turn :none
                         :current-split 0
                         :dealer-wins 0
                         :your-wins 0
                         :result ""})

(def game (atom game-initial-state))
(def shoe (atom (generate-shoe)))
(def hands (atom {}))
(def draw-counter (atom 4))

(defn generate-hands [local-deck]
  (let [your-card-1 (first local-deck)
        dealer-card-1 (second local-deck)
        your-card-2 (local-deck 2)
        dealer-card-2 (local-deck 3)]
    {:you [{:card-1 your-card-1, :card-2 your-card-2}]
     :dealer [{:card-1 dealer-card-1, :card-2 dealer-card-2}]}))

(defn deal! []
  (do
    ;; (reset! shoe (generate-shoe))
    (reset! shoe (cards.deck/generate-specific-shoe [{:suit 's :rank 14} {:suit 'd :rank 2} {:suit 'c :rank 14}]))
    (reset! hands (generate-hands @shoe))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :result "")))

(defn reset-game! []
  (do
    (reset! shoe generate-shoe)
    (reset! hands {})
    (reset! draw-counter 4)
    (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@shoe @draw-counter)))

(defn you-win! []
  (swap! game assoc :state :stopped :turn :none :your-wins (inc (@game :your-wins))))

(defn dealer-wins! []
  (swap! game assoc :state :stopped :turn :none :dealer-wins (inc (@game :dealer-wins))))

(defn update-game! []
  (let [your-value (value (nth (@hands :you) (@game :current-split)))
        dealer-value (value (nth (@hands :dealer) (@game :current-split)))
        dealers-turn-p (= (@game :turn) :dealer)]
    (letfn [(update-result! [result-text]
              (swap! game assoc :result result-text))
            (conclude [winner result-text]
              (do (update-result! result-text)
                  (if (= winner :you) (you-win!) (dealer-wins!))))]
      (cond (= your-value 21) (conclude :you "you win - blackjack = )")
            (> your-value 21) (conclude :dealer "dealer wins - you bust")
            (> dealer-value 21) (conclude :you "you win - dealer busts")
            (= dealer-value 21) (conclude :dealer "dealer wins - blackjack")
            (and dealers-turn-p (> dealer-value your-value)) (conclude :dealer "dealer wins - higher value")
            (and dealers-turn-p (>= dealer-value dealer-hit-cutoff)) (conclude :you "you win - dealer cut off")))))

(defn add-hit-card-to-hand! [player card]
  (do
    (swap! hands assoc-in [player (@game :current-split) :hits]
           (vec (conj ((nth (@hands player) (@game :current-split)) :hits) card)))
    (update-game!)))

(defn stand! []
  (do
    (swap! game assoc :turn :dealer)
    (while (= (@game :state) :running)
      (if (< (value (nth (:dealer @hands) (@game :current-split))) dealer-hit-cutoff)
        (add-hit-card-to-hand! :dealer (draw-hit-card!))
        (update-game!)))))

(defn split! []
  (println "split!"))

(defn game-status [{:keys [state turn dealer-wins your-wins result]} game]
  [:div
   [:ul
    [:li "state: " state]
    [:li "turn: " turn]
    [:li "your wins: " your-wins]
    [:li "dealer wins: " dealer-wins]
    [:li "result: " result]]])

(defn blackjack []
  [:div
   [game-status @game]
   [:div.two-button
    [:button {:on-click #(deal!)} "deal"]
    [:button {:class (if (= (@game :state) :stopped) "inactive") :on-click #(reset-game!)} "reset"]]
   [:div.hand.dealer
    [:h2 "dealer"]
    (for [{:keys [card-1 card-2 hits] :as hand} (@hands :dealer)]
      ^{:key card-1} [card-row (value hand) hand card-1 card-2 hits])]
   [:div.hand.you
    [:h2 "you"]
    (for [{:keys [card-1 card-2 hits] :as hand} (@hands :you)]
      ^{:key card-1} [card-row (value hand) hand card-1 card-2 hits])]
   (let [active-p (and (= (@game :turn) :you) (= (@game :state) :running))
         {:keys [card-1 card-2], :or {card-1 {:suit 's :rank 2} card-2 {:suit 'c :rank 2}}}
         (nth (@hands :you) (@game :current-split))
         can-split-p (= (card-1 :rank) (card-2 :rank))]
     [:div.controls {:class (if (not active-p) "inactive")}
      [:button {:on-click #(add-hit-card-to-hand! :you (draw-hit-card!))} "hit"]
      [:button {:on-click #(stand!)} "stand"]
      [:button {:class (if (not can-split-p) "inactive") :on-click #(split!)} "split"]])])
