(ns cards.components
  (:require
   [cards.deck :as deck]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(defn game-status [{:keys [state turn your-wins dealer-wins pushes current-winner current-split results]} game]
  [:div.game-status
   [:p "your wins: " your-wins]
   [:p "dealer wins: " dealer-wins]
   [:p "pushes: " pushes]
   [:p "state: " state]
   [:p "turn: " turn]
   [:p "current-winner: " current-winner]
   [:p "current-split: " current-split]
   [:p "results: " (apply str (interpose ", " results))]])

;; (defn card-component [{:keys [suit rank]} & [down-p]]
;;   (if down-p
;;     [:span.card {:key (str suit rank)} "X"]
;;     [:span.card {:key (str suit rank)}
;;      (svgs/svg-of suit)
;;      (deck/translate-rank-of rank)]))

(defn card-component [{:keys [suit rank]} & [down-p]]
  (if down-p
    ;; TODO make the back of card
    [:span.card "X"]
    (let [suit-svg (svgs/svg-of suit)
          t-rank (deck/translate-rank-of rank)
          is-face (and (> rank 10) (< rank 14))
          is-ace (= rank 14)
          is-red (or (= suit 'diamond) (= suit 'heart))]
      [:span.card-container
       [:span.card-left (svgs/svg-rank rank is-red) suit-svg]
       [:span.card-middle {:class ["suit-svgs" (str "rank-" rank)]}
        (cond
          is-ace [:span suit-svg]
          is-face [:span.face-card-middle
                   (svgs/svg-face suit rank)
                   [:span.face-card-suit-left suit-svg]
                   [:span.face-card-suit-right suit-svg]]
          (= rank 9) [:<>
                      (into [:span.card-middle-left] (repeat 4 suit-svg))
                      [:span.card-middle-middle suit-svg]
                      (into [:span.card-middle-right] (repeat 4 suit-svg))]
          (= rank 10) [:<>
                       (into [:span.card-middle-left] (repeat 4 suit-svg))
                       (into [:span.card-middle-middle] (repeat 2 suit-svg))
                       (into [:span.card-middle-right] (repeat 4 suit-svg))]
          :else (into [:<>] (repeat rank suit-svg)))]
       [:span.card-right (svgs/svg-rank rank is-red) suit-svg]])))

(defn card-hand-old [card-1 card-2 hits & [down-p]]
  [:div.card-hand
   (card-component card-1 down-p)
   ;; (when card-2 [card card-2])
   ;; (for [hit-card hits]
   ;;   (card-component hit-card))
   ])

(defn hand-component [{:keys [card-1 card-2 hits] :as hand}]
  [:div.card-hand
   ;; "hi"
   (card-component card-1)
   (when card-2 (card-component card-2))
   ;; (for [hit-card hits] (card-component hit-card))
   ])
;; (defn card-hand [{:keys [card-1 card-2 hits]} & [down-p]]
;;   [:div.card-hand
;;    (card-component card-1 down-p)
;;    (if card-2 [card-component card-2])
;;    (for [hit-card hits]
;;      (card-component hit-card))])

(defn card-row [value card-1 card-2 hits active-p & [down-p]]
  [:div.card-row {:class (if active-p "active")}
   (when (not down-p) [:p.val value])
   ;; (card-hand-old card-1 card-2 hits down-p)
   ])

(defn card-list []
  (let [deck (deck/generate-deck)]
    [:div.card-list-container

     [:div.card-list-controls
      [:button {:on-click #(swap! deck deck/shuffle-deck)} "shuffle"]
      [:button {:on-click #(swap! deck deck/generate-deck)} "sort"]]

     ;; (card-component (first deck))
     ;; (card-component {:suit 'spade :rank 12})
     ;; (->> deck (take 1) #(card-component %))
     (into [:div.card-list] (map card-component deck))
     ;; (into [:div.card-list] (map card-component (take 9 deck)))

     [:div
      (svgs/svg-of 's)
      (svgs/svg-of 'c)
      (svgs/svg-of 'd)
      (svgs/svg-of 'h)]]))
