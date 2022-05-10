(ns cards.core-test
  (:require
   [cards.deck :as deck]
   [cards.blackjack-helpers :refer [hands->win-lose-push]]
   [cljs.test :refer-macros [are deftest is testing]]))

(deftest sorted-deck-test
  (is (= true true)))

;; "h-" stands for "hand-", or "sample-test-hand-"
(def h-blackjack [{:suit 'spade, :rank 14} {:suit 'spade, :rank 13}])
(def h-20 [{:suit 'club, :rank 10} {:suit 'club, :rank 10}])
(def h-21 [{:suit 'heart, :rank 10} {:suit 'club, :rank 9} {:suit 'club, :rank 2}])
(def h-22 [{:suit 'diamond, :rank 9} {:suit 'diamond, :rank 8} {:suit 'diamond, :rank 5}])

;; (combo/selections #{"h-20" "h-blackjack" "h-22"} 2) => possible hands
(deftest hands->win-lose-push-test
  (are [x y] (= x y)

    (hands->win-lose-push h-blackjack h-blackjack) :push
    (hands->win-lose-push h-blackjack h-20) :win
    (hands->win-lose-push h-blackjack h-21) :win
    (hands->win-lose-push h-blackjack h-22) :win

    (hands->win-lose-push h-20 h-blackjack) :lose
    (hands->win-lose-push h-20 h-20) :push
    (hands->win-lose-push h-20 h-21) :lose
    (hands->win-lose-push h-20 h-22) :win

    (hands->win-lose-push h-21 h-blackjack) :lose
    (hands->win-lose-push h-21 h-20) :win
    (hands->win-lose-push h-21 h-21) :push
    (hands->win-lose-push h-21 h-22) :win

    (hands->win-lose-push h-22 h-blackjack) :lose
    (hands->win-lose-push h-22 h-20) :lose
    (hands->win-lose-push h-22 h-21) :lose
    (hands->win-lose-push h-22 h-22) :lose))
