(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [sum]]
   [cards.components :as components]
   [cards.deck :refer [generate-shoe]]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(defn generate-hands [local-deck]
  (let [y-d-1-card (first local-deck)
        y-d-2-card (second local-deck)
        d-d-card (local-deck 2)
        d-u-card (local-deck 3)]
    {:you {:card-1 y-d-1-card, :card-2 y-d-2-card}
     :dealer {:card-1 d-d-card, :card-2 d-u-card}}))

(def game (atom {:state :stopped :turn :you :dealer-wins 0 :you-wins 0} ))
(def shoe (atom (generate-shoe)))
(def hands (atom {}))
(def draw-counter (atom 4))

(defn start-game! []
  (do
    (reset! shoe (generate-shoe))
    (reset! hands (generate-hands @shoe))
    (reset! draw-counter 4)
    (swap! game assoc :state :running)))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@shoe @draw-counter)))

(defn update-game! []
  (let [your-sum (sum :you @hands)
        dealer-sum (sum :dealer @hands)]
    (cond (> your-sum 21)
          ;; bust
          (swap! game assoc :state :stopped :dealer-wins (inc (@game :dealer-wins))))))

(defn add-hit-card-to-hand! [player card]
  (swap! hands assoc-in [player :hits] (vec (conj (-> @hands player :hits) card)))
  (update-game!))

(defn end-turn! []
  (swap! game assoc :turn :dealer))

(defn game-status [{:keys [state turn dealer-wins you-wins]} game]
  [:div
   [:p "state: " state]
   [:p "turn: " turn]
   [:p "you wins: " you-wins]
   [:p "dealer wins: " dealer-wins]])

(defn blackjack []
  (do
    ;; (println "blackjack - hands" hands)
    [:div
     [game-status @game]
     [:button {:on-click #(start-game!)} "start game"]
     [:div.dealer
      [:p "dealer"]
      [components/card (-> @hands :dealer :card-1)]
      [components/card (-> @hands :dealer :card-2)]]
     [:div.you
      [:p "you"]
      [components/card (-> @hands :you :card-1)]
      [components/card (-> @hands :you :card-2)]
      [:div
       (for [hit-card (-> @hands :you :hits)]
         [:span
          {:key (rand-int 100000)}
          (components/card hit-card)])]]
     (if (and (= (@game :turn) :you) (= (@game :state) :running))
       [:<>
        [:button {:on-click #(add-hit-card-to-hand! :you (draw-hit-card!))} "hit"]
        [:button {:on-click #(end-turn!)} "stay"]])]))
