(ns cards.core-test
  (:require
   [cards.deck :as deck]
   [cards.blackjack-helpers :refer [hands->win-lose-push]]
   [cljs.test :refer-macros [are deftest is testing]]))

(def top-card {:suit 'heart :rank 14})
(def bottom-card {:suit 'spade :rank 14})

(deftest sorted-deck-test
  (are [x y] (= x y)
    (count (deck/sorted-deck)) 52
    (first (deck/sorted-deck)) top-card
    (last (deck/sorted-deck)) bottom-card))

(deftest generate-shoe-test
  (are [x y] (= x y)
    (count (deck/generate-shoe 1)) 52
    (count (deck/generate-shoe 2)) 104
    (count (deck/generate-shoe 4)) 208
    (first (deck/generate-shoe 4)) top-card
    (-> (deck/generate-shoe 4) (nth 51)) bottom-card
    (-> (deck/generate-shoe 4) (nth 52)) top-card
    (last (deck/generate-shoe 4)) bottom-card))

(deftest riffle-shuffle-test
  ;; two perfect shuffles will match
  (is (= (deck/riffle-shuffle (deck/sorted-deck) 1) (deck/riffle-shuffle (deck/sorted-deck) 1)))
  ;; two imperfect shuffles will not match
  (is (not= (deck/riffle-shuffle (deck/sorted-deck) 0.9) (deck/riffle-shuffle (deck/sorted-deck) 0.9)))
  (are [x y] (= x y)
    (count (deck/riffle-shuffle (deck/sorted-deck))) 52))

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
