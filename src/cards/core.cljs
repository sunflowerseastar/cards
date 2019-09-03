(ns ^:figwheel-hooks cards.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(def cards (atom (for [suit ['s 'c 'd 'h] rank [2 3 4 5 6 7 8 9 10 11 12 13 14]] {:suit suit :rank rank})))

(defn get-app-element []
  (gdom/getElement "app"))

(defn hello-world []
  [:div
   [:ul
    (for [card @cards] [:p (:suit card) " " (:rank card)])]])

(defn mount [el]
  (reagent/render-component [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
