(ns cards.card-display
  (:require
   [cards.components :as c]
   [cards.db :as db]
   [cards.deck :as deck]
   [reagent.core :as reagent :refer [atom]]))

(defn card-display-inner
  "Show the cards, along with buttons to shuffle and sort."
  []
  (let [local-deck (atom (deck/sorted-deck))]
    (fn []
      [:div.card-display.padding-lr-sm.max-width-900
       [:div.card-display-controls
        [:button {:on-click #(swap! local-deck deck/riffle-shuffle)} "riffle"]
        [:button {:on-click #(swap! local-deck deck/cut-deck)} "cut"]
        [:button {:on-click #(reset! local-deck (deck/sorted-deck))} "sort"]]

       ;; (card-component (first local-deck))
       ;; (card-component {:suit 'spade :rank 12})
       ;; (into [:div.card-display] (map card-down-component @local-deck)) ;; all cards are face down
       ;; (into [:div.card-display] (map card-down-component (take 1 @local-deck))) ;; 1 card, face down
       (into [:div.card-display-inner] (map c/card-component @local-deck))])))

(defn card-display-view []
  [card-display-inner])
