(ns cards.card-display
  (:require
   [cards.components :as c]
   [cards.options :as options]
   [cards.db :as db]
   [cards.deck :as deck]
   [reagent.core :refer [atom]]))

(defonce local-shoe (atom (deck/generate-shoe @options/num-decks-in-shoe)))

(defn regenerate!
  "Reset the card-display local-deck to contain an unsorted shoe."
  []
  (reset! local-shoe (deck/generate-shoe @options/num-decks-in-shoe)))

(defn card-display-view
  "Show the cards, along with buttons to shuffle and sort."
  []
  [:div.card-display.padding-lr-sm.max-width-900

   [:div.card-display-controls
    [:div.button-group
     [:button {:class (if (> @options/num-decks-in-shoe 2) "inactive") :on-click #(swap! local-shoe deck/riffle)} "riffle"]
     [:button {:class (if (> @options/num-decks-in-shoe 2) "inactive") :on-click #(swap! local-shoe deck/strip)} "strip"]
     [:button {:class (if (< @options/num-decks-in-shoe 3) "inactive") :on-click #(swap! local-shoe deck/strip)} "shuffle shoe"]]
    [:div.button-group
     [:button {:on-click #(swap! local-shoe deck/cut)} "cut"]
     [:button {:on-click #(swap! local-shoe deck/cut-one-third-top)} "cut top 1/3"]
     [:button {:on-click #(swap! local-shoe deck/cut-one-third-bottom)} "box"]
     [:button {:on-click regenerate!} "reset"]]

    (options/shuffle-precision-slider)
    (options/num-decks-in-shoe-slider regenerate!)

    [:p "Cards shown: " (count @local-shoe)]]

       ;; (card-component (first local-shoe))
       ;; (card-component {:suit :spade :rank 12})
       ;; (into [:div.card-display] (map card-down-component @local-shoe)) ;; all cards are face down
       ;; (into [:div.card-display] (map card-down-component (take 1 @local-shoe))) ;; 1 card, face down
   (into [:div.card-display-inner] (map c/card-component @local-shoe))])
