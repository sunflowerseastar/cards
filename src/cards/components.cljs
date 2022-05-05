(ns cards.components
  (:require
   [goog.string :as gstring]
   [cards.blackjack-helpers :refer [hand->value]]
   [cards.deck :as deck]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

;; --------------------
;; game play components
;; --------------------

(defn stacked-squares
  "A single instance of the repeating square card-back design."
  [n]
  [:span.stacked-squares
   [:span.stacked-squares-inner
    [:span.square-wrapper
     [:div.square]
     [:div.square.one]
     [:div.square.two]
     [:div.square.three]
     [:div.square.four]]
    [:span.square-wrapper.offset
     [:div.square]
     [:div.square.one]
     [:div.square.two]
     [:div.square.three]
     [:div.square.four]]]])

(defn card-down-component
  "Return a card that is face down and doesn't reveal its suit or value."
  []
  [:span.card-container
   [:span.face-down-wrapper
    (into [:span.face-down-inner]
          (let [i (atom 0)]
            (repeatedly 63 (fn []
                             (stacked-squares (even? (swap! i inc)))))))]])

(defn card-component
  "Given a 'card' state, return markup of that card."
  [{:keys [suit rank]}]
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
     [:span.card-right (svgs/svg-rank rank is-red) suit-svg]]))

(defn hand-meta-component
  "Given a hand value and options, render the hand meta/state."
  [hand-value {:keys [is-active hand-outcome is-a-card-in-the-hole]}]
  [:div.hand-meta
   [:span (if is-active "active" (gstring/unescapeEntities "&nbsp;"))]
   [:span {:class (when (> hand-value 21) :is-bust)} (if (not is-a-card-in-the-hole) hand-value "")]
   [:span (if hand-outcome (name hand-outcome) (gstring/unescapeEntities "&nbsp;"))]])

(defn hand-component
  "Given a hand, render hand meta options (state), and the cards themselves."
  [hand & {:as opts}]
  (let [hand-value (hand->value hand)
        {:keys [is-active hand-outcome is-a-card-in-the-hole]} opts]
    [:div.hand {:class (when is-active "is-hand-active")}
     (hand-meta-component hand-value opts)
     (into [:<>]
           (map-indexed
            (fn [i card]
              (if (and is-a-card-in-the-hole (= i 1))
                (card-down-component)
                (card-component card)))
            hand))]))

;; ----------------
;; other components
;; ----------------

(defn card-display []
  (let [local-deck (atom (deck/generate-deck))]
    (fn [] [:div.main.card-display

            [:div.card-display-controls
             [:button {:on-click #(swap! local-deck deck/shuffle-deck)} "shuffle"]
             [:button {:on-click #(reset! local-deck (deck/generate-deck))} "sort"]]

            ;; (card-component (first local-deck))
            ;; (card-component {:suit 'spade :rank 12})
            ;; (into [:div.card-display] (map card-down-component @local-deck)) ;; all cards are face down
            ;; (into [:div.card-display] (map card-down-component (take 1 @local-deck))) ;; 1 card, face down
            (into [:div.card-display] (map card-component @local-deck))])))

(defn game-state-component [game reset-modal-fn]
  [:div.game-state-component
   (into [:div] (map (fn [[k v]] [:p k ": " (str v)]) game))
   [:button {:on-click #(reset-modal-fn)} "reset"]])
