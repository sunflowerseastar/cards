(ns cards.options
  (:require
   [alandipert.storage-atom :refer [local-storage]]
   [reagent.core :refer [atom]]))

(def shuffle-precision-default 0.6)
(defonce shuffle-precision (local-storage (atom shuffle-precision-default) :shuffle-precision))

(def dealer-stands-on-17-default true)
(defonce dealer-stands-on-17 (local-storage (atom dealer-stands-on-17-default) :dealer-stands-on-17))

(defn reset-options-defaults!
  "Clear user changes to defaults."
  []
  (do (reset! dealer-stands-on-17 dealer-stands-on-17-default)
      (reset! shuffle-precision shuffle-precision-default)))



(defn range-slider [value min max step title]
  [:<>
   [:p.slider-text.text-small title ": " @value]
   [:input {:type "range" :value @value :min min :max max :step step
            :style {:width "100%"}
            :on-change (fn [e]
                         (reset! value (js/parseFloat (.. e -target -value))))}]])

(defn shuffle-precision-range-slider []
  [range-slider shuffle-precision 0.0 1.0 "any" "Shuffle Precision"])
