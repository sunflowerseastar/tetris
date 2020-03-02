(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.helpers :refer [piece-can-move-down-p]]
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(defn get-app-element []
  (gdom/getElement "app"))

(def board-width 10)
(def board-height 20)

(defn generate-board []
  (vec (repeat board-height (vec (repeat board-width {})))))

(def game-initial-state {:state :stopped
                         :block-is-falling false
                         :board (generate-board)})

(def game (atom game-initial-state))

(defn add-piece! []
  (do (swap! game assoc-in [:board 0 0] {:color :green :active true :x 0 :y 0})
      (swap! game assoc-in [:board 0 1] {:color :green :active true :x 1 :y 0})
      (swap! game assoc-in [:board 1 0] {:color :green :active true :x 0 :y 1})
      (swap! game assoc-in [:board 1 1] {:color :green :active true :x 1 :y 1})))

(defn start! []
  (do (reset! game game-initial-state)
      (swap! game assoc :state :running)
      (swap! game assoc :block-is-falling true)
      (add-piece!)))

(defn move-active-piece-down! []
  (let [{:keys [board]} @game
        active-squares (filter #(= (:active %) true) (flatten board))
        active-xs-ys (map (fn [square] [(:x square) (:y square)]) active-squares)]
    (do (loop [xs-ys active-xs-ys]
          (if (not (empty? xs-ys))
            (let [x-y (first xs-ys)
                  x (first x-y) y (second x-y)]
              (do (swap! game assoc-in [:board y x] {})
                  (recur (rest xs-ys))))))
        (loop [xs-ys (map (fn [x-y] [(first x-y) (inc (second x-y))]) active-xs-ys)]
          (if (not (empty? xs-ys))
            (let [x-y (first xs-ys)
                  x (first x-y) y (second x-y)]
              (do (swap! game assoc-in [:board y x] {:color :green :active true :x x :y y})
                  (recur (rest xs-ys))))))
        ;; (swap! game assoc :state :stopped)
        )))

(defn tick! []
  (when (= (@game :state) :running)
    (if (= (@game :block-is-falling) false) (add-piece!)
        (when (piece-can-move-down-p (@game :board) board-height) (move-active-piece-down!)))))

(defn tetris []
  (create-class
   {:component-did-mount (fn [] (do
                                  (start!)
                                  (js/setInterval #(tick!) 300)))
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
                                      :class [color "hi"]
                                      :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
                                     [:span.piece-container]]))
                                row))
                             board)]]
                          [:div.button-container
                           [:button {:on-click #(start!)} "start"]]]]))}))

(defn mount [el]
  (reagent/render-component [tetris] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
