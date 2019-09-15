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

(def shoe (atom (deck/generate-shoe)))
(def hands (atom (deck/generate-hands @shoe)))
(def draw-counter (atom 4))

(defn reset-state! []
  (reset! shoe (deck/shuffle-deck (deck/generate-deck)))
  (reset! hands (deck/generate-hands @shoe))
  (reset! draw-counter (atom 4)))

(defn get-hit-card! [player]
  (let [top-card (@shoe @draw-counter)]
    (do
      (println top-card)
      (swap! draw-counter inc)
      top-card)))

(defn add-hit-card! [player card]
  (swap! hands assoc-in [player :hits] (conj (-> @hands player :hits) card)))

(defn blackjack []
  (do
    ;; (println hands)
    ;; (println (count shoe))
    ;; (println hands)
    [:div
     [:button {:on-click #(reset-state!)} "re-deal"]
     [:div.dealer
      [:p "dealer"]
      [card @hands :dealer :down-card]
      [card @hands :dealer :up-card]]
     [:div.you
      [:p "you"]
      [card @hands :you :down-1-card]
      [card @hands :you :down-2-card]
      [:div
       (for [hit-card (-> @hands :you :hits)]
         (do
           ;; (println "dfasdfasd")
           [:span
            {:key (rand-int 100000)}
            (svgs/svg-of (:suit hit-card))
            (deck/translate-rank-of (:rank hit-card))])) ]]
     [:button {:on-click #(add-hit-card! :you (get-hit-card! :you))} "hit"]]))

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
