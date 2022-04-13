(ns cards.blackjack-helpers
  (:require [tupelo.core :refer [spyx]]))

(defn hand->flat-hand
  ;; TODO just make hands flat in the first place
  "Take a player hand and return a 'flat-hand',
  e.g. [{:card-1 ...} {:card-2 ...} {:hits [...]}] -> [{:suit 's :rank 14} ...]"
  [hand]
  (let [{:keys [card-1 card-2 hits]} hand]
    (vec (filter not-empty (flatten (conj [card-1] [card-2] hits))))))

(defn rank->value [rank]
  (cond (< rank 11) rank
        (< rank 14) 10
        (= rank 14) 1))

(defn flat-hand->soft-value
  "Calculate soft hand total (even if over 21)."
  [flat-hand]
  (loop [[{:keys [rank]} & xs] flat-hand is-first-ace-in-hand true acc 0]
    (cond (nil? rank) acc
          (and (= rank 14) is-first-ace-in-hand) (recur xs false (+ acc 11))
          :else (recur xs is-first-ace-in-hand (+ acc (rank->value rank))))))

(defn hand->value
  "Determine hand value, which will be soft if possible."
  [hand]
  (let [flat-hand (hand->flat-hand hand)
        soft-value (flat-hand->soft-value flat-hand)
        hard-value (reduce + (map #(rank->value (% :rank)) flat-hand))]
    ;; TODO - move hand calulation logic to somewhere else
    (if (<= soft-value 21) soft-value hard-value)))
