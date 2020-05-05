(ns ^:figwheel-hooks cards.core
  (:require
   [cards.components :as components]
   [cards.svgs :as svgs]
   [cards.deck :as deck]
   [cards.blackjack :as blackjack]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(def screen (atom blackjack/blackjack))

(defonce has-initially-loaded (atom false))

(defn app []
  (create-class
   {:component-did-mount
    (fn [] (js/setTimeout #(reset! has-initially-loaded true) 0))
    :reagent-render
    (fn [this]
      [:div.app.fade-in-1 {:class [(if @has-initially-loaded "has-initially-loaded")]}
       ;; [:div.two-button.white-bg
       ;;  [:button {:on-click #(reset! screen components/card-list)} "card-list"]
       ;;  [:button {:on-click #(reset! screen blackjack/blackjack)} "blackjack"]]
       [@screen]])}))

(defn mount [el]
  (reagent/render-component [app] el))

(defn get-app-element []
  (gdom/getElement "app-container"))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
