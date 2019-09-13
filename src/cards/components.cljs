(ns cards.components
  (:require
   [cards.svgs :as svgs]
   [cards.deck :as deck]))

(defn selection []
  [:div
   [:p "selection2"]])

(defn blackjack []
  [:div
   (svgs/svg-of 's)
   (svgs/svg-of 'c)
   (svgs/svg-of 'd)
   (svgs/svg-of 'h)
   [:button {:on-click #(swap! deck/deck deck/shuffler deck/weighted-shuffle)} "shuffle"]
   [:button {:on-click #(swap! deck/deck deck/generate-deck)} "sort"]
   [:ul
    (for [card @deck/deck] [:p {:key (apply str [(:suit card) (:rank card)])}
                        (svgs/svg-of (:suit card)) " " (deck/translated-rank-of (:rank card))])]])
