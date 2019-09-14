(ns cards.components
  (:require
   [cards.svgs :as svgs]
   [cards.deck :as deck]
   [reagent.core :as reagent :refer [atom]]))

(defn card [hands player up-or-down]
  (let [{{{suit :suit, rank :rank} up-or-down} player} hands]
    [:span
     (svgs/svg-of suit)
     (deck/translate-rank-of rank)]))

(defn generate-hands [local-deck]
  (let [y-d-1-card (first local-deck)
        y-d-2-card (second local-deck)
        d-d-card (local-deck 2)
        d-u-card (local-deck 3)]
    {:you {:down-1-card y-d-1-card, :down-2-card y-d-2-card}
     :dealer {:down-card d-d-card, :up-card d-u-card}}))

(defn blackjack []
  (let [hands (atom (generate-hands (deck/shuffle-deck (deck/generate-deck))))]
    [:div
     [:button {:on-click #(swap! hands (generate-hands (deck/shuffle-deck (deck/generate-deck))))} "re-deal"]
     [:div.dealer
      [:p "dealer"]
      [card @hands :dealer :down-card]
      [card @hands :dealer :up-card]]
     [:div.you
      [:p "you"]
      [card @hands :you :down-1-card]
      [card @hands :you :down-2-card]
      ]]))

(defn card-list []
  (let [local-deck (atom (deck/generate-deck))]
    (fn []
      [:div
       (svgs/svg-of 's)
       (svgs/svg-of 'c)
       (svgs/svg-of 'd)
       (svgs/svg-of 'h)
       [:button {:on-click #(swap! local-deck deck/shuffle-deck)} "shuffle"]
       [:button {:on-click #(swap! local-deck deck/generate-deck)} "sort"]
       [:ul
        (for [current-card @local-deck] [:p {:key (apply str [(:suit current-card) (:rank current-card)])}
                                         (svgs/svg-of (:suit current-card)) " " (deck/translate-rank-of (:rank current-card))])]])))
