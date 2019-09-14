(ns ^:figwheel-hooks cards.core
  (:require
   [cards.components :as components]
   [cards.svgs :as svgs]
   [cards.deck :as deck]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(def screen (atom components/blackjack))

(defn app []
  [:div
   [:button {:on-click #(reset! screen components/selection)} "go to selection"]
   [:button {:on-click #(reset! screen components/card-list)} "go to card-list"]
   [:button {:on-click #(reset! screen components/blackjack)} "go to blackjack"]
   [@screen]])

(defn mount [el]
  (reagent/render-component [app] el))

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
