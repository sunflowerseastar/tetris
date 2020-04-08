(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.helpers :refer [board->rotated-active-xs-ys
                           get-actives
                           piece-can-move-down-p
                           piece-can-move-left-p
                           piece-can-move-right-p
                           piece-can-rotate-p
                           random-up-to]]
   [goog.dom :as gdom]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn get-app-element []
  (gdom/getElement "app"))

(def board-width 10)
(def board-height 20)
(def colors [:blue :green :red :orange :yellow :purple])
;; (def piece-types [:square :straight :l1 :l2])
(def piece-types [:straight :square])

(defn generate-board []
  (vec (repeat board-height (vec (repeat board-width nil)))))

(def game-initial-state {:state :stopped
                         :active-piece-type nil
                         :active-piece-color nil
                         :board (generate-board)})

(def game (atom game-initial-state))

(defn add-piece! []
  (let [color (get colors (random-up-to (count colors)))
        piece-type (get piece-types (random-up-to (count piece-types)))]
    (do
      (swap! game assoc :active-piece-color color)
      (swap! game assoc :active-piece-type piece-type)
      (cond
        (= piece-type :square)
        (do (swap! game assoc-in [:board 0 4] {:color color :active true :x 4 :y 0})
            (swap! game assoc-in [:board 0 5] {:color color :active true :x 5 :y 0})
            (swap! game assoc-in [:board 1 4] {:color color :active true :x 4 :y 1})
            (swap! game assoc-in [:board 1 5] {:color color :active true :x 5 :y 1}))
        (= piece-type :straight)
        (do (swap! game assoc-in [:board 0 4] {:color color :active true :x 4 :y 0})
            (swap! game assoc-in [:board 1 4] {:color color :active true :x 4 :y 1})
            (swap! game assoc-in [:board 2 4] {:color color :active true :x 4 :y 2})
            (swap! game assoc-in [:board 3 4] {:color color :active true :x 4 :y 3}))
        (= piece-type :l1)
        (do (swap! game assoc-in [:board 0 4] {:color color :active true :x 4 :y 0})
            (swap! game assoc-in [:board 1 4] {:color color :active true :x 4 :y 1})
            (swap! game assoc-in [:board 1 5] {:color color :active true :x 5 :y 1})
            (swap! game assoc-in [:board 1 6] {:color color :active true :x 6 :y 1}))
        (= piece-type :l2)
        (do (swap! game assoc-in [:board 0 5] {:color color :active true :x 5 :y 0})
            (swap! game assoc-in [:board 1 5] {:color color :active true :x 5 :y 1})
            (swap! game assoc-in [:board 1 4] {:color color :active true :x 4 :y 1})
            (swap! game assoc-in [:board 1 3] {:color color :active true :x 3 :y 1}))))))

(defn remove-actives! []
  (let [{:keys [board]} @game
        active-squares (filter #(= (:active %) true) (flatten board))
        active-xs-ys (map (fn [square] [(:x square) (:y square)]) active-squares)]
    (loop [xs-ys active-xs-ys]
      (when (not (empty? xs-ys))
        (let [x-y (first xs-ys)
              x (first x-y) y (second x-y)]
          (do (swap! game assoc-in [:board y x] nil)
              (recur (rest xs-ys))))))))

(defn place-xs-ys-as-actives! [xs-ys]
  (do
    (remove-actives!)
    (loop [xs-ys xs-ys]
      (when (not (empty? xs-ys))
        (let [x-y (first xs-ys) x (first x-y) y (second x-y)]
          (do (swap! game assoc-in [:board y x] {:color (:active-piece-color @game) :active true :x x :y y})
              (recur (rest xs-ys))))))))

(defn rotate! []
  (spyx "rotate!")
  (let [{:keys [active-piece-type board]} @game
        new-xs-ys (board->rotated-active-xs-ys active-piece-type board)]
    (place-xs-ys-as-actives! new-xs-ys)))

(defn start! []
  (do (reset! game game-initial-state)
      (swap! game assoc :state :running)
      (add-piece!)))

(defn move-piece! [x-fn y-fn]
  (let [{:keys [board]} @game
        active-squares (filter #(= (:active %) true) (flatten board))
        active-xs-ys (map (fn [square] [(:x square) (:y square)]) active-squares)
        new-active-xs-ys (map (fn [x-y] [(x-fn (first x-y)) (y-fn (second x-y))]) active-xs-ys)]
    (place-xs-ys-as-actives! new-active-xs-ys)))

(defn move-active-piece-down! []
  (move-piece! identity inc))

(defn move-right! []
  (move-piece! inc identity))

(defn move-left! []
  (move-piece! dec identity))

(defn deactivate-piece! []
  (let [deactivated-board (vec (map (fn [row]
                                      (vec (map (fn [sq]
                                                  (if (:active sq) (assoc sq :active false) sq))
                                                row)))
                                    (@game :board)))]
    (swap! game assoc :board deactivated-board)))

(defn tick! []
  (when (= (@game :state) :running)
    (if (piece-can-move-down-p (@game :board) board-height)
      (move-active-piece-down!)
      (do
        (deactivate-piece!)
        (add-piece!)))))

(defn tetris []
  (letfn [(keyboard-listeners [e]
            (let [is-space (= (.-keyCode e) 32)
                  is-down (= (.-keyCode e) 40)
                  is-left (= (.-keyCode e) 37)
                  is-right (= (.-keyCode e) 39)]
              (cond is-space (if
                                 (piece-can-rotate-p (:active-piece-type @game) (:board @game))
                               (do
                                 (spyx "yes rotate")
                                 (rotate!))
                               (spyx "no rotate"))
                    is-down (tick!)
                    is-left (when (piece-can-move-left-p (:board @game)) (move-left!))
                    is-right (when (piece-can-move-right-p (:board @game) board-width) (move-right!)))))]
    (create-class
     {:component-did-mount (fn [] (do
                                    (start!)
                                    (.addEventListener js/document "keydown" keyboard-listeners)
                                    ;; (js/setInterval #(tick!) 300)
                                    ))
      :reagent-render (fn [this]
                        (let [{:keys [board state]} @game
                              is-stopped (= state :stopped)]
                          [:div#app
                           [:div.tetris
                            [:div.board-container
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
                            [:div.button-container
                             [:button {:on-click #(start!)} "start"]]]]))})))

(defn mount [el]
  (reagent/render-component [tetris] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
