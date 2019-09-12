(ns ^:figwheel-hooks cards.core
  (:require
   [cards.svgs :as svgs]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(defn generate-cards []
  (for [suit ['s 'c 'd 'h] rank [2 3 4 5 6 7 8 9 10 11 12 13 14]] {:suit suit :rank rank}))

(def cards (atom (generate-cards)))

(defn translated-rank-of [rank]
  (case rank
    11 'J
    12 'Q
    13 'K
    14 'A
    rank))

(defn weighted-shuffle [a b]
  (loop [a a b b l [] probably-a 50]
    (let [r (rand-int 100)]
      (cond (and (empty? a) (empty? b)) l
            (empty? a) (concat l b)
            (empty? b) (concat l a)
            (< r probably-a) (recur (rest a) b (conj l (first a)) (- probably-a 15))
            :else (recur a (rest b) (conj l (first b)) (+ probably-a 15))))))

(defn divide-deck [cards]
  (let [separate-point (+ (/ (count cards) 2) (- (rand-int 10) 5))]
    [(take separate-point cards) (drop separate-point cards)]))

(defn shuffler [cards shuffle-fn]
  (let [l-r-cards (divide-deck cards)] (shuffle-fn (first l-r-cards) (second l-r-cards))))

(defn Card-list []
  [:div
   (svgs/svg-of 's)
   (svgs/svg-of 'c)
   (svgs/svg-of 'd)
   (svgs/svg-of 'h)
   [:button {:on-click #(swap! cards shuffler weighted-shuffle)} "shuffle"]
   [:button {:on-click #(swap! cards generate-cards)} "sort"]
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
