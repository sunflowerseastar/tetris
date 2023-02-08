(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.components :refer [board
                              level-component
                              rows-completed-component
                              upcoming-piece-component]]
   [tetris.helpers :refer [board->board-without-actives
                           board->board-without-completions
                           board->rotated-active-xys
                           board->shifted-down-active-xys
                           board->shifted-left-active-xys
                           board->shifted-right-active-xys
                           board-has-4-in-a-row?
                           generate-board
                           piece-can-move-down?
                           piece-can-move-left?
                           piece-can-move-right?
                           piece-can-rotate?
                           xs-ys-are-free?]]
   [goog.dom :as gdom]
   [tupelo.core :refer [append it->]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defonce colors {:lavender "#d0d0ff"
                 :orange "#ffd3ad"
                 :green "#b1e597"
                 :pink "#ffbad1"
                 :red "#ff8c94"
                 :blue "#91cdf2"
                 :yellow "#faedb9"})

(def rows-per-level-up 10)

(defonce has-initially-loaded (atom false))

(defonce board-width 10)
(defonce board-y-negative-offset 2)
(defonce board-height 20)
(defonce queue-length 1)
(defonce tick-duration-multiplier 0.9)
(defonce base-pieces [{:piece-type :square
                       :color-rgb-hex (:lavender colors)
                       :xs-ys [[0 0] [1 0] [0 1] [1 1]]}
                      {:piece-type :straight
                       :color-rgb-hex (:orange colors)
                       :xs-ys [[0 0] [1 0] [2 0] [3 0]]}
                      {:piece-type :s1
                       :color-rgb-hex (:green colors)
                       :xs-ys [[0 1] [1 1] [1 0] [2 0]]}
                      {:piece-type :s2
                       :color-rgb-hex (:pink colors)
                       :xs-ys [[0 0] [1 0] [1 1] [2 1]]}
                      {:piece-type :l1
                       :color-rgb-hex (:red colors)
                       :xs-ys [[0 0] [1 0] [2 0] [2 1]]}
                      {:piece-type :l2
                       :color-rgb-hex (:blue colors)
                       :xs-ys [[0 0] [1 0] [2 0] [0 1]]}
                      {:piece-type :t
                       :color-rgb-hex (:yellow colors)
                       :xs-ys [[0 0] [1 0] [2 0] [1 1]]}])

(defn pieces->offset-pieces [pieces board-width]
  (let [x-offset (-> board-width (quot 2) (- 1))
        static-y-offset board-y-negative-offset]
    (letfn [(add-offsets [xs-ys x-offset-adjusted]
              (map (fn [[x y]] [(+ x-offset-adjusted x) (+ static-y-offset y)]) xs-ys))]
      (mapv #(if (= (:piece-type %) :straight)
               (update % :xs-ys add-offsets (dec x-offset))
               (update % :xs-ys add-offsets x-offset))
            pieces))))

(defonce offset-pieces (pieces->offset-pieces base-pieces board-width))

(defn generate-piece-queue []
  (repeatedly queue-length #(rand-nth offset-pieces)))

(defonce game-initial-state {:is-paused true
                             :active-piece-type nil
                             :active-piece-color nil
                             :game-over true
                             :rows-completed 0
                             :piece-queue nil
                             :level 1
                             :bump-level false
                             :tick-duration 700
                             :board (generate-board board-width (+ board-height board-y-negative-offset))})

(defonce tick-interval (atom 0))
(defonce down-touch-interval (atom 0))
(defonce left-touch-interval (atom 0))
(defonce right-touch-interval (atom 0))

(defonce game (atom game-initial-state))

(defn bump-queue! []
  (let [upcoming-piece (if (board-has-4-in-a-row? (:board @game))
                         (get offset-pieces 1)
                         (rand-nth offset-pieces))]
    (swap! game update :piece-queue #(it-> % (drop 1 it) (append it upcoming-piece)))))

(defn xs-ys->place-actives! [xs-ys]
  (when (seq xs-ys)
    (let [x-y (first xs-ys) x (first x-y) y (second x-y)]
      (swap! game assoc-in [:board y x] {:color (:active-piece-color @game) :active true :x x :y y})
      (xs-ys->place-actives! (rest xs-ys)))))

(defn remove-actives! []
  (swap! game update :board board->board-without-actives))

(defn xs-ys->replace-actives! [xs-ys]
  (remove-actives!)
  (xs-ys->place-actives! xs-ys))

(defn handle-completed-rows! []
  (let [[board-without-completions num-completions]
        (board->board-without-completions (:board @game) board-width)
        rows-completed (:rows-completed @game)
        is-level-up (not= (quot rows-completed rows-per-level-up) (quot (+ rows-completed num-completions) rows-per-level-up))]
    (when (pos? num-completions)
      (swap! game update :rows-completed + num-completions)
      (when is-level-up
        (swap! game update :level inc)
        (swap! game update :tick-duration
               #(* (reduce * (repeat (:level @game) tick-duration-multiplier)) %))
        (swap! game assoc :bump-level true))
      (swap! game assoc :board board-without-completions))))

(defn end-game! []
  (swap! game assoc :game-over true)
  (js/clearInterval @tick-interval)
  (swap! game assoc :is-paused true))

(defn add-piece! []
  (let [{:keys [color-rgb-hex piece-type xs-ys]} (first (:piece-queue @game))
        color color-rgb-hex
        new-piece-overlaps-existing-squares (not (xs-ys-are-free? xs-ys (:board @game)))]
    (bump-queue!)
    (swap! game assoc :active-piece-color color)
    (swap! game assoc :active-piece-type piece-type)
    (xs-ys->place-actives! xs-ys)
    (when new-piece-overlaps-existing-squares (end-game!))))

(defn rotate! []
  (let [{:keys [active-piece-type board]} @game
        new-xs-ys (board->rotated-active-xys active-piece-type board)]
    (xs-ys->replace-actives! new-xs-ys)))

(defn deactivate-piece! []
  (let [deactivated-board
        (vec (map (fn [row]
                    (vec (map (fn [sq]
                                (if (:active sq)
                                  (assoc sq :active false) sq))
                              row)))
                  (:board @game)))]
    (swap! game assoc :board deactivated-board)))

(defn move-active-piece-down! []
  (xs-ys->replace-actives!
   (board->shifted-down-active-xys (:board @game))))

(defn move-left! []
  (xs-ys->replace-actives!
   (board->shifted-left-active-xys (:board @game))))

(defn move-right! []
  (xs-ys->replace-actives!
   (board->shifted-right-active-xys (:board @game))))

(defn tick! []
  (when (not (:game-over @game))
    (if (piece-can-move-down? (:board @game))
      (move-active-piece-down!)
      (do (deactivate-piece!)
          (handle-completed-rows!)
          (when (:bump-level @game)
            (js/clearInterval @tick-interval)
            (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game)))
            (swap! game assoc :bump-level false))
          (add-piece!)))))

(defn start-tick-interval! []
  (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game))))

(defn unpause! []
  (start-tick-interval!)
  (swap! game assoc :is-paused false))

(defn pause-or-unpause! []
  (if (:is-paused @game)
    (unpause!)
    (do (js/clearInterval @tick-interval)
        (swap! game assoc :is-paused true))))

(defn start-game! []
  (reset! game game-initial-state)
  (swap! game assoc :piece-queue (generate-piece-queue))
  (swap! game assoc :game-over false)
  (add-piece!)
  (unpause!))

(defn tetris []
  (letfn [(keyboard-listeners [e]
            (let [is-space (= (.-keyCode e) 32)
                  is-enter (= (.-keyCode e) 13)
                  is-up (= (.-keyCode e) 38)
                  is-down (= (.-keyCode e) 40)
                  is-left (= (.-keyCode e) 37)
                  is-right (= (.-keyCode e) 39)
                  is-p (= (.-keyCode e) 80)
                  is-r (= (.-keyCode e) 82)
                  is-d (= (.-keyCode e) 68)
                  is-n (= (.-keyCode e) 78)
                  is-paused (:is-paused @game)
                  is-game-over (:game-over @game)
                  is-running (and (not is-paused) (not is-game-over))]
              (cond (or is-enter is-space) (if is-game-over (start-game!)
                                               (when (and (not is-paused)
                                                          (piece-can-rotate? (:active-piece-type @game) (:board @game)))
                                                 (rotate!)))
                    is-up (when (and is-running (piece-can-rotate? (:active-piece-type @game) (:board @game))) (rotate!))
                    is-p (when (not is-game-over) (pause-or-unpause!))
                    is-n (when (not is-game-over) (bump-queue!))
                    (or is-d is-down) (when is-running (tick!))
                    is-left (when (and is-running (piece-can-move-left? (:board @game))) (move-left!))
                    is-right (when (and is-running (piece-can-move-right? (:board @game))) (move-right!))
                    is-r (do (end-game!) (start-game!)))))]
    (create-class
     {:component-did-mount
      (fn [] (do (start-game!)
                 (js/setTimeout #(reset! has-initially-loaded true) 0)
                 (.addEventListener js/document "keydown" keyboard-listeners)))
      :reagent-render
      (fn []
        [:div.main.fade-in-1 {:class [(if @has-initially-loaded "has-initially-loaded")]}
         ;; hit area overlay
         (let [is-paused (:is-paused @game)
               is-game-over (:game-over @game)
               is-running (and (not is-paused) (not is-game-over))]
           [:div.hit-area-container {:on-click #(when (and (not is-running)) (start-game!))}
            [:div.hit-area-row
             [:div.hit-area-up {:on-click #(when (and is-running (piece-can-rotate? (:active-piece-type @game) (:board @game))) (rotate!))}]]
            [:div.hit-area-row
             [:div.hit-area-left
              {:on-touch-start #(reset! left-touch-interval
                                        (do
                                          (when (and is-running (piece-can-move-left? (:board @game))) (move-left!))
                                          ;; TODO improve the hold-and-move timing (make it wait for a sec, then start going - a bit faster than prev setting)
                                          (js/setInterval (fn [] (when (and is-running (piece-can-move-left? (:board @game))) (move-left!))) 150)))
               :on-touch-end #(js/clearInterval @left-touch-interval)}]
             [:div.hit-area-down
              {:on-touch-start #(reset! down-touch-interval (js/setInterval (fn [] (tick!)) 50))
               :on-touch-end #(js/clearInterval @down-touch-interval)}]
             [:div.hit-area-right
              {:on-touch-start #(reset! right-touch-interval
                                        (do
                                          (when (and is-running (piece-can-move-right? (:board @game))) (move-right!))
                                          (js/setInterval (fn [] (when (and is-running (piece-can-move-right? (:board @game))) (move-right!))) 150)))
               :on-touch-end #(js/clearInterval @right-touch-interval)}]]])

         ;; gameplay area
         [:div.gameplay-container
          [:div.board-container {:class [(when (:is-paused @game) "is-paused")
                                         (when (:game-over @game) "game-over")]}
           [board board-width game board-y-negative-offset]]
          [:div.meta-container
           [:div.upcoming-piece-container {:class (when (:is-paused @game) "is-paused")}
            [upcoming-piece-component game bump-queue! base-pieces]]
           [:div.rows-completed-container
            [rows-completed-component game bump-queue!]]
           [:div.level-container {:class (when (:is-paused @game) "is-paused")}
            [level-component (:level @game)]]]]])})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [tetris] el)))

(defonce start-up (do (mount-app-element) true))
