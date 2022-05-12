(ns cards.svgs
  (:require
   [cards.svgs-aguilar :as svgs-aguilar]
   [cards.svgs-mine :as svgs-mine]))

(defn svg-of [suit]
  (case suit
    :spade svgs-mine/spade-3
    :club svgs-mine/club-1
    :diamond svgs-mine/diamond-1
    :heart svgs-mine/heart-2
    ""))

(defn svg-face [suit rank]
  (cond
    (= suit :spade)
    (cond
      (= rank 11) svgs-aguilar/spade-jack
      (= rank 12) svgs-aguilar/spade-queen
      (= rank 13) svgs-aguilar/spade-king)
    (= suit :club)
    (cond
      (= rank 11) svgs-aguilar/club-jack
      (= rank 12) svgs-aguilar/club-queen
      (= rank 13) svgs-aguilar/club-king)
    (= suit :diamond)
    (cond
      (= rank 11) svgs-aguilar/diamond-jack
      (= rank 12) svgs-aguilar/diamond-queen
      (= rank 13) svgs-aguilar/diamond-king)
    (= suit :heart)
    (cond
      (= rank 11) svgs-aguilar/heart-jack
      (= rank 12) svgs-aguilar/heart-queen
      (= rank 13) svgs-aguilar/heart-king)))

(defn svg-rank
  "Pass through function to aguilar SVGs."
  [n is-red]
  (svgs-aguilar/rank-svgs n is-red))
