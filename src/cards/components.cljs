(ns cards.components
  (:require
   [cards.svgs :as svgs]
   [cards.deck :as deck]))

(defn selection []
  [:div
   [:p "selection2"]])

(def hands (atom {:dealer {:down-card {:suit 'h :rank 2} :up-card {:suit 'h :rank 9}}
                  :player {:down-card {:suit 'd :rank 10} :up-card {:suit 's :rank 12}}}))

(defn blackjack []
  [:div
   [:button {:on-click #(swap! deck/deck deck/shuffler deck/weighted-shuffle)} "deal"]
   (let [{{{d-u-suit :suit, d-u-rank :rank} :up-card,
           {d-d-suit :suit, d-d-rank :rank} :down-card} :dealer} @hands]
     [:div.dealer
      [:p "dealer"]
      [:span.up-card
       (svgs/svg-of d-u-suit)
       (deck/translate-rank-of d-u-rank)]
      [:span.down-card
       (svgs/svg-of d-d-suit)
       (deck/translate-rank-of d-d-rank)]])
   (let [{{{d-u-suit :suit, d-u-rank :rank} :up-card,
           {d-d-suit :suit, d-d-rank :rank} :down-card} :player} @hands]
     [:div.player
      [:p "player"]
      [:span.up-card
       (svgs/svg-of d-u-suit)
       (deck/translate-rank-of d-u-rank)]
      [:span.down-card
       (svgs/svg-of d-d-suit)
       (deck/translate-rank-of d-d-rank)]])])

(defn card-list []
  [:div
   (svgs/svg-of 's)
   (svgs/svg-of 'c)
   (svgs/svg-of 'd)
   (svgs/svg-of 'h)
   [:button {:on-click #(swap! deck/deck deck/shuffler deck/weighted-shuffle)} "shuffle"]
   [:button {:on-click #(swap! deck/deck deck/generate-deck)} "sort"]
   [:ul
    (for [card @deck/deck] [:p {:key (apply str [(:suit card) (:rank card)])}
                            (svgs/svg-of (:suit card)) " " (deck/translate-rank-of (:rank card))])]])
