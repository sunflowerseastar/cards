(ns cards.blackjack
(:require
 [cards.components :as components]
 [cards.deck :as deck]
 [cards.svgs :as svgs]
 [reagent.core :as reagent :refer [atom]]))

(def shoe (atom (deck/generate-shoe)))
(def hands (atom (deck/generate-hands @shoe)))
(def draw-counter (atom 4))

(defn reset-state! []
  (reset! shoe (deck/shuffle-deck (deck/generate-deck)))
  (reset! hands (deck/generate-hands @shoe))
  (reset! draw-counter 4))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@shoe @draw-counter)))

(defn add-hit-card-to-hand! [player card]
  (swap! hands assoc-in [player :hits] (conj (-> @hands player :hits) card)))

(defn blackjack []
  (do
    (println "blackjack - hands" hands)
    [:div
     [:button {:on-click #(reset-state!)} "re-deal"]
     [:div.dealer
      [:p "dealer"]
      [components/card @hands :dealer :down-card]
      [components/card @hands :dealer :up-card]]
     [:div.you
      [:p "you"]
      [components/card @hands :you :down-1-card]
      [components/card @hands :you :down-2-card]
      [:div
       (for [hit-card (-> @hands :you :hits)]
         (do
           (println "dfasdfasd")
           [:span
            {:key (rand-int 100000)}
            (svgs/svg-of (:suit hit-card))
            (deck/translate-rank-of (:rank hit-card))])) ]]
     [:button {:on-click #(add-hit-card-to-hand! :you (draw-hit-card!))} "hit"]]))
