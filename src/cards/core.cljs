(ns ^:figwheel-hooks cards.core
  (:require
   [cards.svgs :as svgs]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(def cards (atom (for [suit ['s 'c 'd 'h] rank [2 3 4 5 6 7 8 9 10 11 12 13 14]] {:suit suit :rank rank})))

(defn translated-rank-of [rank]
  (case rank
    11 'J
    12 'Q
    13 'K
    14 'A
    rank))

(defn custom-zip
  "Takes two lists and zips them"
  [l m]
  (flatten (map vector l m)))

(defn my-shuffle [cards post-cut-fn]
  (let [cut (+ (/ (count cards) 2) (- (rand-int 10) 5))
        first-half (take cut cards)
        second-half (drop cut cards)] (post-cut-fn first-half second-half)))

(defn Card-list []
  [:div
   (svgs/svg-of 's)
   (svgs/svg-of 'c)
   (svgs/svg-of 'd)
   (svgs/svg-of 'h)
   [:button {:on-click #(swap! @cards (my-shuffle @cards custom-zip))} "shufflex"]
   [:ul
    (for [card @cards] [:p {:key (apply str [(:suit card) (:rank card)])}
                        (svgs/svg-of (:suit card)) " " (translated-rank-of (:rank card))])]])

(defn mount [el]
  (reagent/render-component [Card-list] el))

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
