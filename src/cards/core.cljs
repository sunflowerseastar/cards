(ns ^:figwheel-hooks cards.core
  (:require
   [cards.components :as c]
   [cards.blackjack :refer [blackjack-main]]
   [alandipert.storage-atom :refer [local-storage]]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]
   [reagent.dom :as rdom]))

(defonce has-initially-loaded (atom false))

(def current-route (local-storage (atom :blackjack) :current-route))

(def routes
  {:card-display c/card-display
   :blackjack blackjack-main})

(defn route-to-blackjack!
  "Send user to the blackjack page."
  [] (reset! current-route :blackjack))

(defn route-to-card-display!
  "Send user to the card-display page."
  [] (reset! current-route :card-display))

(defn route-to!
  "Send user to different page."
  [new-route] (reset! current-route new-route))

(defn app []
  (letfn [(keyboard-listeners [e]
            (let [key (.-key e)
                  is-b (= (.-keyCode e) 66)
                  is-d (= (.-keyCode e) 68)]
              (cond is-d (do (println "is-d") (route-to! :card-display))
                    is-b (do (println "is-b") (route-to! :blackjack)))))]
    (create-class
     {:component-did-mount (fn [] (do (js/setTimeout #(reset! has-initially-loaded true) 0)
                                      (.addEventListener js/document "keydown" keyboard-listeners)))
      :reagent-render
      (fn [this]
        (let [current-route-component (@current-route routes)]
          [:div.app.fade-in-1 {:class [(if @has-initially-loaded "has-initially-loaded")]}
           (c/modal current-route routes route-to!)
           (c/header)
           (current-route-component)]))})))

(defn mount [el]
  (rdom/render [app] el))

(defn get-app-element []
  (gdom/getElement "app-container"))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
