(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [hand->value]]
   [cards.components :refer [game-status hand-component card-row]]
   [cards.deck :refer [generate-shuffled-deck]]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(def dealer-hit-cutoff 17)


;; State variables get the "normal" name, and components get the cumbersome long
;; name. Ex. (hand-component hand) is the `hand-component` function receiving a
;; piece of game state called `hand`.
(def game-initial-state {:state :stopped
                         :turn :none
                         :current-split 0
                         :dealer-wins 0
                         :your-wins 0
                         :current-winner nil
                         :pushes 0
                         :is-modal-showing false
                         :results []})

(def game (atom game-initial-state))
;; TODO actually use the shoe... this is a deck, not a shoe
(def deck (atom (generate-shuffled-deck)))
;; Ex. {:you [{:card-1 {:suit diamond, :rank 7}, :card-2 {:suit diamond, :rank 14}}],
;;      :dealer [{:card-1 {:suit heart, :rank 2}, :card-2 {:suit heart, :rank 8}}]}
;; Dealer gets a vector of hands, although in practice the dealer always has only one hand.
(def hands (atom {}))
(def draw-counter (atom 4))

(defn generate-hands [local-deck]
  (let [your-card-1 (first local-deck)
        dealer-card-1 (second local-deck)
        your-card-2 (local-deck 2)
        dealer-card-2 (local-deck 3)]
    {:you [{:card-1 your-card-1, :card-2 your-card-2}]
     :dealer [{:card-1 dealer-card-1, :card-2 dealer-card-2}]}))

(defn reset-game! []
  (do
    (reset! deck generate-shuffled-deck)
    (reset! hands {})
    (reset! draw-counter 4)
    (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@deck @draw-counter)))

(defn end-game! []
  (swap! game assoc :state :stopped :turn :none))

(defn increment-wins! [win-count]
  (swap! game assoc win-count (inc (@game win-count))))

(defn you-win! []
  (swap! game assoc :your-wins (inc (@game :your-wins)) :current-winner :you))

(defn dealer-wins! []
  (swap! game assoc :dealer-wins (inc (@game :dealer-wins)) :current-winner :dealer))

(defn push! []
  (increment-wins! :pushes))

(defn update-result! [result-text]
  (swap! game assoc :results (conj (@game :results) result-text)))

(defn conclude [winner result-text]
  (do
    (update-result! result-text)
    (case winner
      :push (push!)
      :dealer (dealer-wins!)
      (you-win!))))

(defn play-next-split! []
  (swap! game assoc :current-split (inc (@game :current-split))))

(defn conclude-game! []
  (let [your-values (map hand->value (@hands :you))
        dealer-value (hand->value (nth (@hands :dealer) 0))]
    (do
      (doseq [your-value your-values]
        (cond (> your-value 21) (conclude :dealer "dealer wins - you bust")
              (> dealer-value 21) (conclude :you "you win - dealer busts")
              (= your-value dealer-value) (conclude :push "push")
              (= your-value 21) (conclude :you "you win - blackjack :)")
              (= dealer-value 21) (conclude :dealer "dealer wins - blackjack")
              (> dealer-value your-value) (conclude :dealer "dealer wins - higher value")
              :else (conclude :you "you win - higher value")))
      (end-game!))))

(defn add-hit-card-to-hand! [player card]
  (if (and (= (@game :turn) :you) (nil? ((nth (@hands :you) (@game :current-split)) :card-2)))
    (swap! hands assoc-in [player (@game :current-split) :card-2] card)
    (swap! hands assoc-in [player (@game :current-split) :hits]
           (vec (conj ((nth (@hands player) (@game :current-split)) :hits) card)))))

(defn dealer-plays! []
  (do
    (swap! game assoc :turn :dealer :current-split 0)
    (while (= (@game :state) :running)
      (if (< (hand->value (nth (:dealer @hands) 0)) dealer-hit-cutoff)
        (add-hit-card-to-hand! :dealer (draw-hit-card!))
        (conclude-game!)))))

(defn deal! []
  (do
    (reset! deck (generate-shuffled-deck))
    ;; (reset! deck (cards.deck/generate-specific-deck [{:suit 's :rank 14} {:suit 'd :rank 2} {:suit 'c :rank 14}]))
    (reset! hands (generate-hands @deck))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :current-split 0 :current-winner nil :results [])
    (if (= (hand->value (nth (@hands :you) (@game :current-split))) 21) (dealer-plays!))))

(defn stand! []
  (if (> (- (count (@hands :you)) 1) (@game :current-split))
    (play-next-split!)
    (dealer-plays!)))

(defn split! []
  (do
    (swap! hands assoc-in [:you (+ (@game :current-split) 1) :card-1] ((nth (@hands :you) (@game :current-split)) :card-2))
    (swap! hands update-in [:you (@game :current-split)] dissoc :card-2)))

(defn hit! []
  (do
    (add-hit-card-to-hand! :you (draw-hit-card!))
    (let [your-value (hand->value (nth (@hands :you) (@game :current-split)))
          more-splits-remaining-p (and (= (@game :turn) :you) (> (- (count (@hands :you)) 1) (@game :current-split)))]
      (cond (and more-splits-remaining-p (>= your-value 21)) (play-next-split!)
            (>= your-value 21) (dealer-plays!)))))

(defn blackjack []
  [:div.main {:class (if (@game :is-modal-showing) "stats-showing")}
   [:div.stats {:class (if (@game :is-modal-showing) "active")
                :on-click #(swap! game assoc :is-modal-showing (not (@game :is-modal-showing)))}
    [game-status ^{:class "stats"} @game]]
   [:div.logo-container [:a {:on-click #(swap! game assoc :is-modal-showing (not (@game :is-modal-showing)))}

     ]]
   [:div.two-button
    [:button {:on-click #(deal!)} "deal"]
    [:button {:on-click #(reset-game!)} "reset"]]

   [:div.card-play-area
    [:div.player-container.dealer

    [:h2 {:class (if (= (@game :current-winner) :dealer) "win")} "dealer"]

    [:div.hands
     (let [hand (first (:dealer @hands))
           value (hand->value hand)
           is-active false]
       (do
         (println hand)
         (when hand (hand-component hand is-active))))]

    ]

   [:div.player-container.you

    [:h2 {:class (if (= (@game :current-winner) :you) "win")} "you"]

    (into [:div.hands] (map-indexed

                        (fn [i hand]
                          (let [is-active (and (= (@game :turn) :you)
                                               ;; (> (count (@hands :you)) 1)
                                               ;; (= i (@game :current-split))
                                               )
                                ]
                            (hand-component hand is-active))
                           ;; (card-row (hand->value hand) card-1 card-2 hits
                                                   ;; (and (= (@game :turn) :you)
                                                   ;;      (> (count (@hands :you)) 1)
                                                   ;;      (= index (@game :current-split))))
                          )

                        (@hands :you)))

    ;; (into [:div.hands] (map-indexed
    ;;         (fn [index {:keys [card-1 card-2 hits] :as hand}]
    ;;           ^{:key card-1} (card-row (hand->value hand) card-1 card-2 hits
    ;;                                    (and (= (@game :turn) :you)
    ;;                                         (> (count (@hands :you)) 1)
    ;;                                         (= index (@game :current-split)))))
    ;;         (@hands :you)))

    ]]


   (let [active-p (and (= (@game :turn) :you) (= (@game :state) :running))
         {:keys [card-1 card-2 hits], :or {card-1 {} card-2 {} hits []}}
         (nth (@hands :you) (@game :current-split))
         can-stand-p (not-empty card-2)
         can-split-p (and (= (card-1 :rank) (card-2 :rank)) (empty? hits))]
     [:div.three-button {:class (if (not active-p) "inactive")}
      [:button {:on-click #(hit!)} "hit"]
      [:button {:class (if (not can-stand-p) "inactive") :on-click #(stand!)} "stand"]
      [:button {:class (if (not can-split-p) "inactive") :on-click #(split!)} "split"]])])
