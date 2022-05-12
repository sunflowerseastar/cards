(ns cards.components
  (:require
   [cards.blackjack-helpers :refer [hand->value]]
   [cards.db :as db]
   [cards.deck :as deck]
   [cards.options :as options]
   [cards.svgs :as svgs]
   [goog.string :as gstring]
   [reagent.core :as reagent :refer [atom]]))

;; ---------------
;; cards and hands
;; ---------------

(defn square-pattern [classname]
  [:span.square-pattern {:class (when classname classname)}
   [:div.square]
   [:div.square.one]
   [:div.square.two]
   [:div.square.three]
   [:div.square.four]])

(defn stacked-squares
  "A row of the repeating 'stacked squares' card-back design.
  ref. https://codepen.io/ItsMeNatalie/pen/OJLYbrr"
  []
  [:span.stacked-squares>span.stacked-squares-inner
   [square-pattern]
   [square-pattern "offset"]])

(defn card-down-component
  "Return a card that is face down and doesn't reveal its suit or value."
  []
  [:span.card-container>span.face-down-wrapper
   (into [:span.face-down-inner] (repeatedly 63 #(stacked-squares)))])

(defn card-component
  "Given a 'card' state (ex. {:suit :club :rank 3}), return markup of that card."
  [{:keys [suit rank]}]
  (let [suit-svg (svgs/svg-of suit)
        is-face (and (> rank 10) (< rank 14))
        is-ace (= rank 14)
        is-red (or (= suit :diamond) (= suit :heart))]
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
   [:div.meta-value-container {:class (when hand-outcome (name hand-outcome))}
    [:span.meta-value.text-small (if (not is-a-card-in-the-hole) hand-value "")]]
   [:div.small-down-card {:class (when (not is-active) "hide")} (card-down-component)]])

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

;; --
;; UI
;; --

(defn header
  "Top of UI with modal-toggling hamburger."
  []
  [:div.header>a.hamburger-container {:on-click #(db/toggle-modal!)} [:div.hamburger]])

(defn blocker
  "This is the grayed out full-viewport area that sits above the gameplay and
  below the modal."
  []
  [:div.blocker {:class (if (:is-modal-showing @db/game) "is-modal-showing")
                 :on-click #(db/toggle-modal!)}])

(defn close-x
  "X to close modal."
  []
  [:a.close-x-container {:on-click #(db/toggle-modal!)}
   [:span.close-x]])

(defn modal-routing
  "Markup for the modal page-switching."
  [current-route routes route-to-fn]
  [:div.modal-routing
   [:a {:on-click #(when (not= @current-route :blackjack) (route-to-fn :blackjack))
        :class (when (= @current-route :blackjack) "is-active")} "blackjack"]
   " | "
   [:a {:on-click #(when (not= @current-route :card-display) (route-to-fn :card-display))
        :class (when (= @current-route :card-display) "is-active")} "card-display"]])

(defn modal
  "Markup for the modal, blocker, and modal contents."
  [current-route routes route-to-fn]
  [:<>
   [:div.modal {:class (if (:is-modal-showing @db/game) "is-modal-showing")}
    (blocker)
    [:div.modal-inner-scroll-container>div.modal-inner
     (close-x)
     (modal-routing current-route routes route-to-fn)

     [:hr]
     (into [:div] (map (fn [[k v]] [:p k ": " (str v)]) @db/outcomes))

     [:hr]
     [:p "Dealer "
      [:a.inline-toggle {:on-click #(swap! options/dealer-stands-on-17 not)}
       (if @options/dealer-stands-on-17 "stands" "hits")] " on 17"]
     (options/split-non-ace-slider)
     (options/split-ace-slider)
     [:p {:class (when (zero? @options/num-splits-permitted-ace) "light-gray")}
      [:a.inline-toggle {:on-click #(swap! options/can-hit-split-aces not)}
       (if @options/can-hit-split-aces "Can" "Cannot")] " hit split aces"]

     [:hr]
     (options/shuffle-precision-slider)
     (options/num-decks-in-shoe-slider)

     [:hr]

     [:button {:on-click #(options/reset-options-defaults!)} "revert options to defaults"]
     [:button {:on-click #(db/reset-game!)} "reset win/lose/push"]]]])
