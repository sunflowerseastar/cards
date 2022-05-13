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
     [:button {:class (if (> @options/num-decks-in-shoe 2) "inactive") :on-click #(swap! local-shoe deck/riffle-shuffle)} "riffle"]
     [:button {:class (if (> @options/num-decks-in-shoe 2) "inactive") :on-click #(swap! local-shoe deck/strip-shuffle)} "strip"]
     [:button {:on-click #(swap! local-shoe deck/cut-deck)} "cut"]
     [:button {:on-click regenerate!} "regenerate"]]
    (options/shuffle-precision-slider)
    (options/num-decks-in-shoe-slider regenerate!)
    [:p "Cards shown: " (count @local-shoe)]]


       ;; (card-component (first local-shoe))
       ;; (card-component {:suit :spade :rank 12})
       ;; (into [:div.card-display] (map card-down-component @local-shoe)) ;; all cards are face down
       ;; (into [:div.card-display] (map card-down-component (take 1 @local-shoe))) ;; 1 card, face down


   (into [:div.card-display-inner] (map c/card-component @local-shoe))])

;; (defn card-display-view []
;;   [card-display-inner])
