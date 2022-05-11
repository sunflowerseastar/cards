(ns cards.options
  (:require
   [alandipert.storage-atom :refer [local-storage]]
   [reagent.core :refer [atom]]))

(def shuffle-precision-default 0.6)
(defonce shuffle-precision (local-storage (atom shuffle-precision-default) :shuffle-precision))

(def dealer-stands-on-17-default true)
(defonce dealer-stands-on-17 (local-storage (atom dealer-stands-on-17-default) :dealer-stands-on-17))

(def num-splits-permitted-non-ace-default 3)
(defonce num-splits-permitted-non-ace (local-storage (atom num-splits-permitted-non-ace-default) :num-splits-permitted-non-ace))

(def num-splits-permitted-ace-default 1)
(defonce num-splits-permitted-ace (local-storage (atom num-splits-permitted-ace-default) :num-splits-permitted-ace))

(def can-hit-split-aces-default false)
(defonce can-hit-split-aces (local-storage (atom can-hit-split-aces-default) :can-hit-split-aces))

(defn reset-options-defaults!
  "Clear user changes to defaults."
  []
  (do (reset! dealer-stands-on-17 dealer-stands-on-17-default)
      (reset! num-splits-permitted-non-ace num-splits-permitted-non-ace-default)
      (reset! num-splits-permitted-ace num-splits-permitted-ace-default)
      (reset! shuffle-precision shuffle-precision-default)))



(defn range-slider-int [value min max step title]
  [:<>
   [:p.slider-text.text-small title ": " @value]
   [:input {:type "range" :value @value :min min :max max :step step
            :style {:width "100%"}
            :on-change (fn [e]
                         (reset! value (js/parseInt (.. e -target -value))))}]])

(defn split-non-ace-slider []
  [range-slider-int num-splits-permitted-non-ace 0 3 1 "Number of non-ace splits permitted"])

(defn split-ace-slider []
  [range-slider-int num-splits-permitted-ace 0 3 1 "Number of ace splits permitted"])

(defn range-slider-float [value min max step title]
  [:<>
   [:p.slider-text.text-small title ": " @value]
   [:input {:type "range" :value @value :min min :max max :step step
            :style {:width "100%"}
            :on-change (fn [e]
                         (reset! value (js/parseFloat (.. e -target -value))))}]])

(defn shuffle-precision-slider []
  [range-slider-float shuffle-precision 0.0 1.0 "any" "Shuffle Precision"])
