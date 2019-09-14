(ns cards.components
  (:require
   [cards.svgs :as svgs]
   [cards.deck :as deck]
   [reagent.core :as reagent :refer [atom]]))

(def hands (atom {:dealer {:down-card {:suit 'h :rank 2} :up-card {:suit 'h :rank 9}}
                  :you {:down-card {:suit 'd :rank 10} :up-card {:suit 's :rank 12}}}))

(defn card [player up-or-down]
  (let [{{{suit :suit, rank :rank} up-or-down} player} @hands]
    [:span
     (svgs/svg-of suit)
     (deck/translate-rank-of rank)]))

(defn blackjack []
  [:div
   [:div.dealer
    [:p "dealer"]
    [card :dealer :up-card]
    [card :dealer :down-card]]
   [:div.you
    [:p "you"]
    [card :you :up-card]
    [card :you :down-card]
    ]])

(defn card-list []
  (let [local-deck (atom (deck/generate-deck))]
    (fn []
      [:div
       (svgs/svg-of 's)
       (svgs/svg-of 'c)
       (svgs/svg-of 'd)
       (svgs/svg-of 'h)
       [:button {:on-click #(swap! local-deck deck/shuffler deck/weighted-shuffle)} "shuffle"]
       [:button {:on-click #(swap! local-deck deck/generate-deck)} "sort"]
       [:ul
        (for [current-card @local-deck] [:p {:key (apply str [(:suit current-card) (:rank current-card)])}
                                         (svgs/svg-of (:suit current-card)) " " (deck/translate-rank-of (:rank current-card))])]])))
