(ns cards.blackjack
  (:require
   [cards.blackjack-helpers :refer [value]]
   [cards.components :refer [card-hand card-row]]
   [cards.deck :refer [generate-shoe]]
   [cards.svgs :as svgs]
   [reagent.core :as reagent :refer [atom]]))

(def dealer-hit-cutoff 17)

(def game-initial-state {:state :stopped
                         :turn :none
                         :current-split 0
                         :dealer-wins 0
                         :your-wins 0
                         :current-winner nil
                         :pushes 0
                         :show-stats false
                         :results []})

(def game (atom game-initial-state))
(def shoe (atom (generate-shoe)))
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
    (reset! shoe generate-shoe)
    (reset! hands {})
    (reset! draw-counter 4)
    (reset! game game-initial-state)))

(defn draw-hit-card! []
  (do
    (swap! draw-counter inc)
    (@shoe @draw-counter)))

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
  (let [your-values (map value (@hands :you))
        dealer-value (value (nth (@hands :dealer) 0))]
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
      (if (< (value (nth (:dealer @hands) 0)) dealer-hit-cutoff)
        (add-hit-card-to-hand! :dealer (draw-hit-card!))
        (conclude-game!)))))

(defn deal! []
  (do
    (reset! shoe (generate-shoe))
    ;; (reset! shoe (cards.deck/generate-specific-shoe [{:suit 's :rank 14} {:suit 'd :rank 2} {:suit 'c :rank 14}]))
    (reset! hands (generate-hands @shoe))
    (reset! draw-counter 4)
    (swap! game assoc :state :running :turn :you :current-split 0 :current-winner nil :results [])
    (if (= (value (nth (@hands :you) (@game :current-split))) 21) (dealer-plays!))))

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
    (let [your-value (value (nth (@hands :you) (@game :current-split)))
          more-splits-remaining-p (and (= (@game :turn) :you) (> (- (count (@hands :you)) 1) (@game :current-split)))]
      (cond (and more-splits-remaining-p (>= your-value 21)) (play-next-split!)
            (>= your-value 21) (dealer-plays!)))))

(defn game-status [{:keys [state turn your-wins dealer-wins pushes current-winner current-split results]} game]
  [:ul.game-status
   [:li "your wins: " your-wins]
   [:li "dealer wins: " dealer-wins]
   [:li "pushes: " pushes]
   [:li "state: " state]
   [:li "turn: " turn]
   [:li "current-winner: " current-winner]
   [:li "current-split: " current-split]
   [:li "results: " (apply str (interpose ", " results))]])

(defn blackjack []
  [:div.blackjack {:class (if (@game :show-stats) "stats-showing")}
   [:div.stats {:class (if (@game :show-stats) "active")
                :on-click #(swap! game assoc :show-stats (not (@game :show-stats)))}
    [game-status ^{:class "stats"} @game]]
   [:div.logo-container {:on-click #(swap! game assoc :show-stats (not (@game :show-stats)))}
    [:img {:src "images/logo.png"}]]
   [:div.two-button
    [:button {:on-click #(deal!)} "deal"]
    [:button {:on-click #(reset-game!)} "reset"]]
   [:div.hand.dealer
    [:h2 {:class (if (= (@game :current-winner) :dealer) "win")} "dealer"]
    (doall (for [{:keys [card-1 card-2 hits] :as hand} (@hands :dealer)]
             ^{:key card-1}
             [card-row (value hand) hand card-1 card-2 hits
              (= (@game :turn) :dealer)
              (= (@game :turn) :you)]))]
   [:div.hand.you
    [:h2 {:class (if (= (@game :current-winner) :you) "win")} "you"]
    (doall (map-indexed
            (fn [index {:keys [card-1 card-2 hits] :as hand}]
              ^{:key card-1} [card-row (value hand) hand card-1 card-2 hits
                              (and (= (@game :turn) :you)
                                   (> (count (@hands :you)) 1)
                                   (= index (@game :current-split)))])
            (@hands :you)))]
   (let [active-p (and (= (@game :turn) :you) (= (@game :state) :running))
         {:keys [card-1 card-2 hits], :or {card-1 {} card-2 {} hits []}}
         (nth (@hands :you) (@game :current-split))
         can-stand-p (not-empty card-2)
         can-split-p (and (= (card-1 :rank) (card-2 :rank)) (empty? hits))]
     [:div.three-button {:class (if (not active-p) "inactive")}
      [:button {:on-click #(hit!)} "hit"]
      [:button {:class (if (not can-stand-p) "inactive") :on-click #(stand!)} "stand"]
      [:button {:class (if (not can-split-p) "inactive") :on-click #(split!)} "split"]])])
