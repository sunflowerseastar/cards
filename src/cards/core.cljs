(ns ^:figwheel-hooks cards.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(def cards (atom (for [suit ['s 'c 'd 'h] rank [2 3 4 5 6 7 8 9 10 11 12 13 14]] {:suit suit :rank rank})))

(defn get-app-element []
  (gdom/getElement "app"))

(defn svg-of [suit]
  (case suit
    d [:svg {:class "diamond" :viewBox [0 0 3113 3603]}
       [:path {:d ["M1576.7-2.7l-15.4" "21.6c540" "752.2" "1091.8" "1327.5" "1550.8" "1776.5 c-465.8" "455.2-1028" "1040.8-1576.8" "1809.9l15.6-21.8l-16.2-22.5C1005.4" "2827.3" "465.6" "2263" "13.9" "1820.5L0.1" "1807 C461.1" "1356.4" "1016.6" "778.2" "1559.9" "20.8L1576.7-2.7z"]}]]
    h [:svg {:class "heart" :viewBox [0 0 1792 1792]}
        [:path {:d ["M896" "1664q-26" "0-44-18l-624-602q-10-8-27.5-26t-55.5-65.5-68-97.5-53.5-121-23.5-138q0-220" "127-344t351-124q62" "0" "126.5" "21.5t120" "58" "95.5" "68.5" "76" "68q36-36" "76-68t95.5-68.5" "120-58" "126.5-21.5q224" "0" "351" "124t127" "344q0" "221-229" "450l-623" "600q-18" "18-44" "18z"]}]]
    "default"))

(defn hello-world []
  [:div
   (svg-of 'h)
   (svg-of 'd)
   [:ul
    (for [card @cards] [:p (svg-of (:suit card)) " " (:rank card)])]])

(defn mount [el]
  (reagent/render-component [hello-world] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
