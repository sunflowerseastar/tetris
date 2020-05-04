(ns ^:figwheel-hooks tetris.core
  (:require
   [clojure.math.combinatorics :as combo]
   [tetris.helpers :refer [board->board-without-actives
                           board->board-without-completions
                           board->rotated-active-xs-ys
                           board->shifted-down-active-xs-ys
                           board->shifted-left-active-xs-ys
                           board->shifted-right-active-xs-ys
                           board-has-4-in-a-row?
                           generate-blank-row
                           generate-board
                           get-actives
                           piece-can-move-down?
                           piece-can-move-left?
                           piece-can-move-right?
                           piece-can-rotate?
                           random-up-to
                           xs-ys-are-free?]]
   [goog.dom :as gdom]
   [tupelo.core :refer [append it-> spyx]]
   [clojure.core.matrix :refer [new-matrix]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defonce colors {:lavender "#d0d0ff"
                 :orange "#ffd3ad"
                 :green "#b1e597"
                 :pink "#ffbad1"
                 :red "#ff8c94"
                 :blue "#91cdf2"
                 :yellow "#faedb9"})

(def gradient-pairs (combo/combinations colors 2))

(defonce has-initially-loaded (atom false))

(defonce board-width 10)
(defonce board-height 20)
(defonce queue-length 1)
(defonce tick-duration-multiplier 0.9)
(defonce base-pieces [{:piece-type :square
                       :color-rgb-hex (:lavender colors)
                       :xs-ys [[0 0] [1 0] [0 1] [1 1]]}
                      {:piece-type :straight
                       :color-rgb-hex (:orange colors)
                       :xs-ys [[0 0] [0 1] [0 2] [0 3]]}
                      {:piece-type :s1
                       :color-rgb-hex (:green colors)
                       :xs-ys [[0 1] [1 1] [1 0] [2 0]]}
                      {:piece-type :s2
                       :color-rgb-hex (:pink colors)
                       :xs-ys [[0 0] [1 0] [1 1] [2 1]]}
                      {:piece-type :l1
                       :color-rgb-hex (:red colors)
                       :xs-ys [[0 0] [0 1] [1 1] [2 1]]}
                      {:piece-type :l2
                       :color-rgb-hex (:blue colors)
                       :xs-ys [[2 0] [2 1] [1 1] [0 1]]}
                      {:piece-type :t
                       :color-rgb-hex (:yellow colors)
                       :xs-ys [[1 0] [0 1] [1 1] [2 1]]}])

(defn pieces->offset-pieces [pieces board-width]
  (let [offset (-> board-width (quot 2) (- 1))]
    (letfn [(inc-xs-ys [xs-ys] (map (fn [[x y]] [(+ offset x) y]) xs-ys))]
      (map #(update % :xs-ys inc-xs-ys) pieces))))

(defonce offset-pieces (vec (pieces->offset-pieces base-pieces board-width)))

(defn random-base-piece []
  (get offset-pieces (random-up-to (count base-pieces))))

(defn generate-piece-queue []
  (repeatedly queue-length random-base-piece))

(defonce game-initial-state {:is-paused true
                             :active-piece-type nil
                             :active-piece-color nil
                             :game-over true
                             :rows-completed 0
                             :piece-queue nil
                             :level 1
                             :bump-level false
                             :tick-duration 700
                             :board (generate-board board-width board-height)})

(defonce tick-interval (atom 0))

(defonce game (atom game-initial-state))

(defn bump-queue! []
  (let [upcoming-piece (if (board-has-4-in-a-row? (:board @game)) (get offset-pieces 1) (random-base-piece))]
    (swap! game update :piece-queue #(it-> % (drop 1 it) (append it upcoming-piece)))))

(defn xs-ys->place-actives! [xs-ys]
  (when (not (empty? xs-ys))
    (let [x-y (first xs-ys) x (first x-y) y (second x-y)]
      (do (swap! game assoc-in [:board y x] {:color (:active-piece-color @game) :active true :x x :y y})
          (xs-ys->place-actives! (rest xs-ys))))))

(defn remove-actives! []
  (swap! game update :board board->board-without-actives))

(defn xs-ys->replace-actives! [xs-ys]
  (do (remove-actives!)
      (xs-ys->place-actives! xs-ys)))

(defn handle-completed-rows! []
  (let [[board-without-completions num-completions]
        (board->board-without-completions (:board @game) board-width)
        rows-completed (:rows-completed @game)
        is-level-up (not= (quot rows-completed 10) (quot (+ rows-completed num-completions) 10))]
    (when (pos? num-completions)
      (do
        (swap! game update :rows-completed + num-completions)
        (when is-level-up
          (swap! game update :level inc)
          (swap! game update :tick-duration
                 #(* (reduce * (repeat (:level @game) tick-duration-multiplier)) %))
          (swap! game assoc :bump-level true))
        (swap! game assoc :board board-without-completions)))))

(defn end-game! []
  (do (swap! game assoc :game-over true)
      (js/clearInterval @tick-interval)
      (swap! game assoc :is-paused true)))

(defn add-piece! []
  (let [{:keys [color-rgb-hex piece-type xs-ys]} (first (:piece-queue @game))
        color color-rgb-hex
        new-piece-overlaps-existing-squares (not (xs-ys-are-free? xs-ys (:board @game)))]
    (do (bump-queue!)
        (swap! game assoc :active-piece-color color)
        (swap! game assoc :active-piece-type piece-type)
        (xs-ys->place-actives! xs-ys)
        (when new-piece-overlaps-existing-squares (end-game!)))))

(defn rotate! []
  (let [{:keys [active-piece-type board]} @game
        new-xs-ys (board->rotated-active-xs-ys active-piece-type board)]
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
   (board->shifted-down-active-xs-ys (:board @game))))

(defn move-left! []
  (xs-ys->replace-actives!
   (board->shifted-left-active-xs-ys (:board @game))))

(defn move-right! []
  (xs-ys->replace-actives!
   (board->shifted-right-active-xs-ys (:board @game))))

(defn tick! []
  (when (not (:game-over @game))
    (if (piece-can-move-down? (:board @game))
      (move-active-piece-down!)
      (do (deactivate-piece!)
          (handle-completed-rows!)
          (when (:bump-level @game)
            (do (js/clearInterval @tick-interval)
                (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game)))
                (swap! game assoc :bump-level false)))
          (add-piece!)))))

(defn start-tick-interval! []
  (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game))))

(defn unpause! []
  (do (start-tick-interval!)
      (swap! game assoc :is-paused false)))

(defn pause-or-unpause! []
  (if (:is-paused @game)
    (unpause!)
    (do (js/clearInterval @tick-interval)
        (swap! game assoc :is-paused true))))

(defn start-game! []
  (do (reset! game game-initial-state)
      (swap! game assoc :piece-queue (generate-piece-queue))
      (swap! game assoc :game-over false)
      (add-piece!)
      (unpause!)))

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
                  is-paused (:is-paused @game)
                  is-game-over (:game-over @game)
                  is-running (and (not is-paused) (not is-game-over))]
              (cond (or is-enter is-space) (if is-game-over (start-game!)
                                               (when (and (not is-paused)
                                                          (piece-can-rotate? (:active-piece-type @game) (:board @game)))
                                                 (rotate!)))
                    is-up (when (and is-running (piece-can-rotate? (:active-piece-type @game) (:board @game))) (rotate!))
                    is-p (when (not is-game-over) (pause-or-unpause!))
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
      (fn [this]
        [:div.tetris.fade-in-1 {:class [(if @has-initially-loaded "has-initially-loaded")]}
         [:div.row
          [:div.left]
          [:div.center
           [:div.board
            (map-indexed
             (fn [y row]
               (map-indexed
                (fn [x square]
                  (let [{:keys [color]} square]
                    [:div.square
                     {:key (str x y)
                      :class [(when (zero? x) "left-edge") (when (zero? y) "top-edge")]
                      :style {:grid-column (+ x 1) :grid-row (+ y 1)
                              :grid-template-columns "repeat(10, 10%)"
                              :background color}}]))
                row))
             (:board @game))]]
          [:div.right
           [:div.board-mini-container
            [:div.board-mini
             (let [upcoming-piece (first (:piece-queue @game))
                   {:keys [color-rgb-hex piece-type]} upcoming-piece
                   base-xs-ys (->> base-pieces
                                   (filter #(= (:piece-type %) piece-type))
                                   first
                                   :xs-ys)
                   xs (map first base-xs-ys) ys (map second base-xs-ys)
                   min-x (reduce min xs) max-x (reduce max xs) width-x (inc (- max-x min-x))
                   min-y (reduce min ys) max-y (reduce max ys) height-y (inc (- max-y min-y))
                   matrix-for-grid (new-matrix height-y width-x)]
               (map-indexed
                (fn [y row]
                  (map-indexed
                   (fn [x square]
                     (let [match (some #{[x y]} base-xs-ys)]
                       [:div {:key (str x y)
                              :style {:grid-column (+ x 1) :grid-row (+ y 1)
                                      :background (when match color-rgb-hex)}}]))
                   row))
                matrix-for-grid))]]
           [:div.rows-completed-container.fade-in-2
            (map-indexed
             (fn [i gradient-pair]
               [:span.rows-completed
                {:class (if (<= i (rem (:rows-completed @game) (count gradient-pairs))) "in")
                 :style {:background (str "-webkit-linear-gradient(45deg, "
                                          (-> (first gradient-pair) val) ", "
                                          (-> (second gradient-pair) val) " 80%)")
                         :backgroundClip "border-box"
                         :-webkitBackgroundClip "text"
                         :-webkitTextFillColor "transparent"}}
                (:rows-completed @game)])
             gradient-pairs)]
           [:div.level-completed
            [:span.level.fade-in-2 (:level @game)]]]]])})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [tetris] el)))

(defonce start-up (do (mount-app-element) true))
