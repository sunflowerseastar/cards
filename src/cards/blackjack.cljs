(ns cards.blackjack
  (:require
   [cards.components :as components]
   [cards.deck :as deck]
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
(def shoe (atom (deck/generate-shoe)))
(def hands (atom {}))
(def draw-counter (atom 4))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@shoe @draw-counter)))

(defn cards-from-hand [hand]
  (let [{:keys [card-1 card-2 hits]} hand]
    (vec (filter not-empty (flatten (conj [card-1] [card-2] hits))))))

(defn sum [player]
  (let [t1 (map :rank (cards-from-hand (@hands player)))]
    (reduce + (map :rank (cards-from-hand (@hands player))))))

(defn update-game! []
  (let [your-sum (sum :you)
        dealer-sum (sum :dealer)]
    (cond (> your-sum 21)
          ;; bust
          (swap! game assoc :state :stopped :dealer-wins (inc (@game :dealer-wins))))))

(defn end-turn! []
  (swap! game assoc :turn :dealer))

(defn add-hit-card-to-hand! [player card]
  (swap! hands assoc-in [player :hits] (vec (conj (-> @hands player :hits) card)))
  (update-game!))

(defn start-game! []
  (do
    (reset! shoe (deck/shuffle-deck (deck/generate-deck)))
    (reset! hands (generate-hands @shoe))
    (reset! draw-counter 4)
    (swap! game assoc :state :running)
    ))

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
