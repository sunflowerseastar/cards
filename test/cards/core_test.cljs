(ns cards.core-test
  (:require
   [cards.deck :as deck]
   [cards.blackjack-helpers :refer [hands->win-lose-push rs]]
   [cljs.test :refer-macros [are deftest is testing]]))

;; 'rs' is a card shorthand that means 'rank suit'
(deftest rs-test
  (are [x y] (= x y)
    (rs "A c") {:rank 14 :suit :club}
    (rs "14 c") {:rank 14 :suit :club}
    (rs "a c") {:rank 14 :suit :club}
    (rs "13 s") {:rank 13 :suit :spade}
    (rs "k s") {:rank 13 :suit :spade}
    (rs "10 s") {:rank 10 :suit :spade}
    (rs "9 s") {:rank 9 :suit :spade}))

(def top-card (rs "a h"))
(def bottom-card (rs "a s"))

(deftest generate-sorted-deck-test
  (are [x y] (= x y)
    (count (deck/generate-sorted-deck)) 52
    (first (deck/generate-sorted-deck)) top-card
    (last (deck/generate-sorted-deck)) bottom-card))

(deftest plus-minus-test
  (are [x y] (= x y)
    (as-> (repeatedly 10000 #(deck/plus-minus 100 0.9)) xs [(apply min xs) (apply max xs)]) [90 110]
    (as-> (repeatedly 10000 #(deck/plus-minus 100 0.8)) xs [(apply min xs) (apply max xs)]) [80 120]
    (as-> (repeatedly 10000 #(deck/plus-minus 100 0.5)) xs [(apply min xs) (apply max xs)]) [50 150]
    (as-> (repeatedly 10000 #(deck/plus-minus 100 0)) xs [(apply min xs) (apply max xs)]) [0 200]))

(deftest divide-cards-test
  (are [x y] (= x y)
    (->> (deck/divide-cards (deck/generate-sorted-deck) 1) (map count)) '(26 26)))

(deftest riffle-test
  ;; two perfect (faro) riffles will match
  (is (= (deck/riffle (deck/generate-sorted-deck) 1) (deck/riffle (deck/generate-sorted-deck) 1)))

  ;; two imperfect riffles will not match
  (is (not= (deck/riffle (deck/generate-sorted-deck) 0.9) (deck/riffle (deck/generate-sorted-deck) 0.9)))

  ;; 52 perfect (faro) shuffles puts the deck in original order
  (is (= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 52) (deck/generate-sorted-deck)))
  (is (not= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 51) (deck/generate-sorted-deck)))
  (is (not= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 53) (deck/generate-sorted-deck)))

  ;; 26 perfect deck/riffles puts the deck in reverse order
  (is (= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 26) (reverse (deck/generate-sorted-deck))))
  (is (not= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 25) (reverse (deck/generate-sorted-deck))))
  (is (not= (nth (iterate #(deck/riffle % 1) (deck/generate-sorted-deck)) 27) (reverse (deck/generate-sorted-deck))))

  ;; these last two ideas won't work with imprecise shuffles
  (is (not= (nth (iterate #(deck/riffle % 0.99) (deck/generate-sorted-deck)) 52) (deck/generate-sorted-deck)))
  (is (not= (nth (iterate #(deck/riffle % 0.99) (deck/generate-sorted-deck)) 26) (reverse (deck/generate-sorted-deck))))

  (are [x y] (= x y)
    ;; sanity check
    (count (deck/riffle (deck/generate-sorted-deck))) 52))

(deftest strip-test
  ;; two perfect strips will match
  (is (= (deck/strip (deck/generate-sorted-deck) 1) (deck/strip (deck/generate-sorted-deck) 1)))

  ;; two imperfect strips will not match (note that )
  (is (not= (deck/strip (deck/generate-sorted-deck) 0.9) (deck/strip (deck/generate-sorted-deck) 0.9))))

(deftest generate-shoe-test
  (are [x y] (= x y)
    (count (deck/generate-shoe 1)) 52
    (count (deck/generate-shoe 2)) 104
    (count (deck/generate-shoe 4)) 208
    (first (deck/generate-shoe 4)) top-card
    (-> (deck/generate-shoe 4) (nth 51)) bottom-card
    (-> (deck/generate-shoe 4) (nth 52)) top-card
    (last (deck/generate-shoe 4)) bottom-card))

;; "h-" stands for "hand-", or "sample-test-hand-"

;; This isn't a blackjack if you have split hands. Technically, this hand would
;; be 'blackjack for any hand other than you if you have split hands.'
(def h-blackjack [(rs "a s") (rs "k s")])
(def h-20 [(rs "10 c") (rs "11 c")])
(def h-21 [(rs "10 h") (rs "9 c") (rs "2 c")])
(def h-22 [(rs "9 d") (rs "8 d") (rs "5 d")])

;; (combo/selections #{"h-20" "h-blackjack" "h-22"} 2) => possible hands
(deftest hands->win-lose-push-test
  (are [x y] (= x y)

    ;; The final optional 'true' means 'you-have-split-hands' (so again, your
    ;; hand is not technically a blackjack). Hence the expected :lose result.
    (hands->win-lose-push h-blackjack h-blackjack true) :lose
    ;; Final optional bool defaults to 'false', as in 'not a split hand.'
    (hands->win-lose-push h-blackjack h-blackjack) :push
    (hands->win-lose-push h-blackjack h-20) :win
    (hands->win-lose-push h-blackjack h-21) :win
    (hands->win-lose-push h-blackjack h-21 true) :push
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
