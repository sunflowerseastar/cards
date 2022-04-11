(ns cards.components
  (:require
   [cards.deck :as deck]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(defn game-status [{:keys [state turn your-wins dealer-wins pushes current-winner current-split results]} game]
  [:ul.game-status
   [:li "your wins: " your-wins]
   [:li "dealer wins: " dealer-wins]
   [:li "pushes: " pushes]
   [:li "state: " state]
   [:li "turn: " turn]
   [:li "current-winner: " current-winner]
   [:li "current-split: " current-split]
   [:li "results: " (apply str (interpose ", " results))]])

(defn card [{:keys [suit rank]} & [down-p]]
  (if down-p
    [:span.card {:key (str suit rank)} "X"]
    [:span.card {:key (str suit rank)}
     (svgs/svg-of suit)
     (deck/translate-rank-of rank)]))

(defn card-alt [{:keys [suit rank]} & [down-p]]
  (if down-p
    ;; TODO make the back of card
    [:span.card "X"]
    (let [suit-svg (svgs/svg-of suit)
          t-rank (deck/translate-rank-of rank)
          is-face (and (> rank 10) (< rank 14))
          is-ace (= rank 14)]
      [:span.card-container
       [:span.card-left (svgs/svg-number rank) suit-svg]
       [:span.card-middle {:class ["suit-svgs" (str "rank-" rank)]}
        (cond
          is-ace [:span suit-svg]
          is-face [:span suit-svg t-rank]
          (= rank 9) [:<>
                      (into [:span.card-middle-left] (repeat 4 suit-svg))
                      [:span.card-middle-middle suit-svg]
                      (into [:span.card-middle-right] (repeat 4 suit-svg))]
          (= rank 10) [:<>
                       (into [:span.card-middle-left] (repeat 4 suit-svg))
                       (into [:span.card-middle-middle] (repeat 2 suit-svg))
                       (into [:span.card-middle-right] (repeat 4 suit-svg))]
          :else (into [:<>] (repeat rank suit-svg)))]
       [:span.card-right (svgs/svg-number rank) suit-svg]])))

(defn card-hand [card-1 card-2 hits & [down-p]]
  [:div.card-hand
   (card-alt card-1 down-p)
   (if card-2 [card-alt card-2])
   (for [hit-card hits]
     (card-alt hit-card))])

(defn card-row [value card-1 card-2 hits active-p & [down-p]]
  [:div.card-row {:class (if active-p "active")}
   (if (not down-p) [:p.val value])
   (card-hand card-1 card-2 hits down-p)])

(defn card-list []
  (let [deck (deck/generate-deck)]
    [:div.card-list-container

     [:div.card-list-controls
      [:button {:on-click #(swap! deck deck/shuffle-deck)} "shuffle"]
      [:button {:on-click #(swap! deck deck/generate-deck)} "sort"]]

     ;; (card-alt (first deck))
     ;; (->> deck (take 1) #(card-alt %))
     (into [:div.card-list] (map card-alt deck))
     ;; (into [:div.card-list] (map card-alt (take 9 deck)))

     [:div
      (svgs/svg-of 's)
      (svgs/svg-of 'c)
      (svgs/svg-of 'd)
      (svgs/svg-of 'h)]]))
