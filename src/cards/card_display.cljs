(ns cards.card-display
  (:require
   [cards.components :as c]
   [cards.options :as options]
   [cards.db :as db]
   [cards.deck :as deck]
   [reagent.core :refer [atom]]))

(defonce local-deck (atom (deck/sorted-deck)))

;; TODO see if there's something about this component that's not wanting to render..?? not sure
(defn card-display-view
  "Show the cards, along with buttons to shuffle and sort."
  []
  [:div.card-display.padding-lr-sm.max-width-900
   [:div.card-display-controls.button-group
    [:button {:on-click #(swap! local-deck deck/riffle-shuffle)} "riffle"]
    [:button {:on-click #(swap! local-deck deck/strip-shuffle)} "strip"]
    [:button {:on-click #(swap! local-deck deck/cut-deck)} "cut"]
    [:button {:on-click #(reset! local-deck (deck/sorted-deck))} "sort"]]
   [:div.card-display-controls
    (options/shuffle-precision-slider)
    ]

       ;; (card-component (first local-deck))
       ;; (card-component {:suit 'spade :rank 12})
       ;; (into [:div.card-display] (map card-down-component @local-deck)) ;; all cards are face down
       ;; (into [:div.card-display] (map card-down-component (take 1 @local-deck))) ;; 1 card, face down
   (into [:div.card-display-inner] (map c/card-component @local-deck))])

;; (defn card-display-view []
;;   [card-display-inner])
