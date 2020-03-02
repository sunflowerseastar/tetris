(ns ^:figwheel-hooks tetris.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(defn get-app-element []
  (gdom/getElement "app"))

(def board-width 10)
(def board-height 20)

(defn generate-board []
  (vec (repeat board-height (vec (repeat board-width {})))))

(def game-initial-state {:state :stopped
                         :board (generate-board)})

(def game (atom game-initial-state))

(defn start! []
  (do (reset! game game-initial-state)))

(defn tetris []
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
              (let [{:keys [color]} square])
              [:div.square
               {:key (str x y)
                :style {:grid-column (+ x 1) :grid-row (+ y 1)}}
               [:span.piece-container]]) row)) board) ]]
      [:div.button-container
       [:button {:on-click #(start!)} "start"]]]]))

(defn mount [el]
  (reagent/render-component [tetris] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))
