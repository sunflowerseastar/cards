(ns cards.blackjack-helpers
  (:require [tupelo.core :refer [spyx]]))

(defn rank->hard-value [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 1))

(defn hand->value
  "Determine hand value, which will be soft if possible."
  [hand]
  (let [hard-value (reduce + (map #(rank->hard-value (:rank %)) hand))
        soft-value (when (some #(= (:rank %) 14) hand)
                     (+ hard-value 10))]
    (if (and (pos? soft-value) (<= soft-value 21))
      soft-value
      hard-value)))
