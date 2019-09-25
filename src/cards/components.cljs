(ns cards.components
  (:require
   [cards.deck :as deck]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(defn card [{:keys [suit rank]} & [down-p]]
  (if down-p
    [:span.card {:key (str suit rank)} "X"]
    [:span.card {:key (str suit rank)}
     (svgs/svg-of suit)
     (deck/translate-rank-of rank)]))

(defn card-hand [card-1 card-2 hits & [down-p]]
  [:div.card-hand
   [card card-1 down-p]
   (if card-2 [card card-2])
   (for [hit-card hits]
     (card hit-card))])

(defn card-row [value hand card-1 card-2 hits active-p & [down-p]]
  [:div.card-row {:class (if active-p "active")}
   (if (not down-p) [:p.val value])
   [card-hand card-1 card-2 hits down-p]])

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
