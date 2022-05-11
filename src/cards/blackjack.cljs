(ns cards.blackjack
  (:require
   [alandipert.storage-atom :refer [local-storage]]
   [cards.blackjack-helpers :refer [hand->value hands->win-lose-push]]
   [cards.components :as c]
   [cards.db :as db]
   [cards.deck :refer [generate-shuffled-deck]]
   [cards.options :as options]
   [reagent.core :as reagent :refer [atom]]))

;; ---------------
;; gameplay markup
;; ---------------

(defn blackjack-view []
  [:div.blackjack-view.padding-lr-sm.max-width-900
   [:div.card-play-area
    (when (not (empty? @db/hands))
      [:<>

       (let [hand (:dealer @db/hands)
             is-active false
             is-a-card-in-the-hole (= (:turn @db/game) :you)]
         (c/hand-component hand
                           :is-active is-active
                           :is-a-card-in-the-hole is-a-card-in-the-hole))

       [:div.player-division-line>h2 "--- dealer "
        (if options/dealer-stands-on-17 "stands" "hits") " on soft 17 ---"]

       (into [:<>]
             (->> (@db/hands :you)
                  (map-indexed
                   (fn [i hand]
                     (let [is-active (and (= (:turn @db/game) :you)
                                          (= (:current-split @db/game) i))
                           hand-outcome (and (= (:state @db/game) :stopped)
                                             (hands->win-lose-push hand (:dealer @db/hands)))
                           is-win (and (= (:state @db/game) :stopped)
                                       (= hand-outcome :win))]
                       (c/hand-component hand
                                         :is-active is-active
                                         :hand-outcome hand-outcome))))))])]

   [:div.button-group {:class (when (not= (:state @db/game) :stopped) "inactive")}
    [:button {:on-click #(db/deal!)} "deal"]]

   (let [is-active (and (= (:turn @db/game) :you) (= (:state @db/game) :running))
         your-current-hand (nth (:you @db/hands) (:current-split @db/game))
         [card-1 card-2 & hits] your-current-hand
         can-stand (some? card-2)
         num-your-split-hands (->> @db/hands :you count)
         can-split-aces (and
                         (>= @options/num-splits-permitted-ace num-your-split-hands)
                         (= (:rank card-1) (:rank card-2))
                         (= (:rank card-1) 14)
                         (empty? hits))
         can-split-non-aces (and
                             (>= @options/num-splits-permitted-non-ace num-your-split-hands)
                             (= (:rank card-1) (:rank card-2))
                             (not= (:rank card-1) 14)
                             (empty? hits))
         can-split (or can-split-aces can-split-non-aces)
         cannot-hit (and
                     (false? @options/can-hit-split-aces)
                      ;; player can't keep hitting if they're playing split hands...
                     (-> (:you @db/hands) count (> 1))
                      ;; ...and it is aces that were split
                     (= (->> your-current-hand first :rank) 14)
                      ;; ...and they've already hit once.
                     (= (count your-current-hand) 2))]
     [:div.button-group {:class (if (not is-active) "inactive")}
      [:button {:class (if cannot-hit "inactive") :on-click #(db/hit!)} "hit"]
      [:button {:class (if (not can-stand) "inactive") :on-click #(db/stand!)} "stand"]
      [:button {:class (if (not can-split) "inactive") :on-click #(db/split!)} "split"]])])
