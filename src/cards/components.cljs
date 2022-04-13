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

(defn card-component
  "Given a 'card' state, return markup of that card."
  [{:keys [suit rank]} & [down-p]]
  (if down-p
    ;; TODO make the back of card
    ;; TODO move the back of card to a different function (lift down-p)
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

(defn hand-component
  "Given a hand, show hand meta (state) and the cards themselves."
  [{:keys [card-1 card-2 hits] :as hand} is-active]
  [:div.hand {:class (if is-active "is-hand-active")}
   (card-component card-1)
   (when card-2 (card-component card-2))
   (into [:<>] (map #(card-component %) hits))])

;; --------------------------------
;; DISPLAY is not part of game play
;; --------------------------------

(defn card-display []
  (let [local-deck (atom (deck/generate-deck))]
    (fn [] [:div.main.card-display

            [:div.card-display-controls
             [:button {:on-click #(swap! local-deck deck/shuffle-deck)} "shuffle"]
             [:button {:on-click #(reset! local-deck (deck/generate-deck))} "sort"]]

            ;; (card-component (first local-deck))
            ;; (card-component {:suit 'spade :rank 12})
            (into [:div.card-display] (map card-component @local-deck))])))
