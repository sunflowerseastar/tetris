(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.helpers :refer [
                           board->board-without-completions
                           board->rotated-active-xs-ys
                           board->shifted-down-active-xs-ys
                           board->shifted-left-active-xs-ys
                           board->shifted-right-active-xs-ys
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
   [reagent.core :as reagent :refer [atom create-class]]))

(defonce board-width 10)
(defonce board-height 20)
(defonce queue-length 5)
(defonce piece-types [:square :straight :s1 :s2 :l1 :l2 :t])
(defonce base-pieces [{:piece-type :square
                       :color-rgb-hex "#d0d0ff"
                       :xs-ys [[0 0] [1 0] [0 1] [1 1]]}
                      {:piece-type :straight
                       :color-rgb-hex "#ffd3ad"
                       :xs-ys [[1 0] [1 1] [1 2] [1 3]]}
                      {:piece-type :s1
                       :color-rgb-hex "#b1e597"
                       :xs-ys [[0 1] [1 1] [1 0] [2 0]]}
                      {:piece-type :s2
                       :color-rgb-hex "#b9e5a1"
                       :xs-ys [[0 0] [1 0] [1 1] [2 1]]}
                      {:piece-type :l1
                       :color-rgb-hex "#ff8c94"
                       :xs-ys [[0 0] [0 1] [1 1] [2 1]]}
                      {:piece-type :l2
                       :color-rgb-hex "#91cdf2"
                       :xs-ys [[2 0] [2 1] [1 1] [0 1]]}
                      {:piece-type :t
                       :color-rgb-hex "#faedb9"
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

(defonce game-initial-state {:state :stopped
                             :active-piece-type nil
                             :active-piece-color nil
                             :game-over true
                             :rows-completed 0
                             :piece-queue nil
                             :board (generate-board board-width board-height)})

(defonce game (atom game-initial-state))

(defn bq [piece-queue]
  (it-> piece-queue (drop 1 it) (append it (random-base-piece))))

(defn bump-queue! []
  (swap! game update :piece-queue bq))

(defn xs-ys->place-actives! [xs-ys]
  (when (not (empty? xs-ys))
    (let [x-y (first xs-ys) x (first x-y) y (second x-y)]
      (do (swap! game assoc-in [:board y x] {:color (:active-piece-color @game) :active true :x x :y y})
          (xs-ys->place-actives! (rest xs-ys))))))

(defn remove-actives! []
  (loop [actives (get-actives (:board @game))]
    (when (not (empty? actives))
      (let [active (first actives)]
        (do (swap! game assoc-in [:board (:y active) (:x active)] nil)
            (recur (rest actives)))))))

(defn xs-ys->replace-actives! [xs-ys]
  (do (remove-actives!)
      (xs-ys->place-actives! xs-ys)))

(defn handle-completed-rows! []
  (let [[new-board num-completions]
        (board->board-without-completions (:board @game) board-width)]
    (when (pos? num-completions)
      (do (swap! game update :rows-completed inc)
          (swap! game assoc :board new-board)))))

(defn get-initial-piece-xs-ys [piece-type]
  (cond (= piece-type :square) [[4 0] [5 0] [4 1] [5 1]]
        (= piece-type :straight) [[4 0] [4 1] [4 2] [4 3]]
        (= piece-type :s1) [[4 1] [5 1] [5 0] [6 0]]
        (= piece-type :s2) [[4 0] [5 0] [5 1] [6 1]]
        (= piece-type :l1) [[4 0] [4 1] [5 1] [6 1]]
        (= piece-type :l2) [[5 0] [5 1] [4 1] [3 1]]
        (= piece-type :t) [[4 0] [3 1] [4 1] [5 1]]))

(defn end-game! []
  (do (swap! game assoc :game-over true)
      (swap! game assoc :state :stopped)))

(defn pause-game! []
  (let [current-state (:state @game)]
    (swap! game assoc :state (if (= current-state :stopped) :running :stopped))))

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

(defn start! []
  (do (reset! game game-initial-state)
      (swap! game assoc :piece-queue (generate-piece-queue))
      (swap! game assoc :state :running)
      (swap! game assoc :game-over false)
      (add-piece!)))

(defn move-active-piece-down! []
  (xs-ys->replace-actives!
   (board->shifted-down-active-xs-ys (:board @game))))

(defn move-left! []
  (xs-ys->replace-actives!
   (board->shifted-left-active-xs-ys (:board @game))))

(defn move-right! []
  (xs-ys->replace-actives!
   (board->shifted-right-active-xs-ys (:board @game))))

(defn deactivate-piece! []
  (let [deactivated-board
        (vec (map (fn [row]
                    (vec (map (fn [sq]
                                (if (:active sq)
                                  (assoc sq :active false) sq))
                              row)))
                  (@game :board)))]
    (swap! game assoc :board deactivated-board)))

(defn tick! []
  (when (and (not (@game :game-over))
             (= (:state @game) :running))
    (if (piece-can-move-down? (@game :board))
      (move-active-piece-down!)
      (do
        (deactivate-piece!)
        (handle-completed-rows!)
        (add-piece!)))))

(defn tetris []
  (letfn [(keyboard-listeners [e]
            (let [is-space (= (.-keyCode e) 32)
                  is-down (= (.-keyCode e) 40)
                  is-left (= (.-keyCode e) 37)
                  is-right (= (.-keyCode e) 39)
                  is-p (= (.-keyCode e) 80)
                  is-running (= (:state @game) :running)
                  is-game-over (:game-over @game)]
              (cond is-space (cond
                               (and is-running (piece-can-rotate? (:active-piece-type @game) (:board @game)))
                               (rotate!)
                               is-game-over (start!))
                    is-p (pause-game!)
                    is-down (when is-running (tick!))
                    is-left (when (and is-running (piece-can-move-left? (:board @game))) (move-left!))
                    is-right (when (and is-running (piece-can-move-right? (:board @game))) (move-right!)))))]
    (create-class
     {:component-did-mount
      (fn [] (do (start!)
                 (.addEventListener js/document "keydown" keyboard-listeners)
                 (js/setInterval #(tick!) 300)))
      :reagent-render
      (fn [this]
        (let [{:keys [board state]} @game
              is-stopped (= state :stopped)]
          [:div#app
           [:div.tetris
            [:div.board-container
             [:div.board-container-left
              [:div.board
               (map-indexed
                (fn [y row]
                  (map-indexed
                   (fn [x square]
                     (let [{:keys [color]} square]
                       [:div.square
                        {:key (str x y)
                         :style {:grid-column (+ x 1) :grid-row (+ y 1)
                                 :background color}}
                        [:span.piece-container]]))
                   row))
                board)]]
             [:div.board-container-right
              [:p.rows-completed
               {:style {:color (:active-piece-color @game)}}
               (:rows-completed @game)]
              [:p.speed 1]]]]]))})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [tetris] el)))

;; https://figwheel.org/docs/hot_reloading.html#re-rendering-ui-after-saving-a-file
;; (defn ^:after-load re-render []
;;   (mount-app-element))
(defonce start-up (do (mount-app-element) true))
