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
      (cond
        is-ace [:span.card-alt suit-svg t-rank]
        is-face [:span.card-alt suit-svg t-rank]
        :else
        [:span.card-alt (repeat rank suit-svg) t-rank]))))

(defn card-hand [card-1 card-2 hits & [down-p]]
  [:div.card-hand
   [card-alt card-1 down-p]
   (if card-2 [card-alt card-2])
   (for [hit-card hits]
     (card-alt hit-card))])

(defn card-row [value card-1 card-2 hits active-p & [down-p]]
  [:div.card-row {:class (if active-p "active")}
   (if (not down-p) [:p.val value])
   (card-hand card-1 card-2 hits down-p)])

(defn card-list []
  (let [local-deck (atom (deck/generate-deck))]
    (fn []
      [:div.card-list-container
       [:div.card-list-2
        "hi"
        [:div.hand-2
         (card-alt {:suit 's :rank 14})
         (card-hand {:suit 'c :rank 2} {:suit 'd :rank 5} [])
         ;; (card-row 10 {:suit 'h :rank 11} {:suit 'd :rank 4} [] nil)
         ]]

       [:div.card-list
        (svgs/svg-of 's)
        (svgs/svg-of 'c)
        (svgs/svg-of 'd)
        (svgs/svg-of 'h)
        [:button {:on-click #(swap! local-deck deck/shuffle-deck)} "shuffle"]
        [:button {:on-click #(swap! local-deck deck/generate-deck)} "sort"]
        [:ul
         (for [current-card @local-deck] [:p {:key (apply str [(:suit current-card) (:rank current-card)])}
                                          (svgs/svg-of (:suit current-card)) " " (deck/translate-rank-of (:rank current-card))])]]])))
