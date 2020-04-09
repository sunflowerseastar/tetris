(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.helpers :refer [board->rotated-active-xs-ys
                           board->shifted-down-active-xs-ys
                           board->shifted-left-active-xs-ys
                           board->shifted-right-active-xs-ys
                           get-actives
                           piece-can-move-down?
                           piece-can-move-left?
                           piece-can-move-right?
                           piece-can-rotate?
                           random-up-to]]
   [goog.dom :as gdom]
   [tupelo.core :refer [spyx]]
   [reagent.core :as reagent :refer [atom create-class]]))

(defonce board-width 10)
(defonce board-height 20)
(defonce colors [:blue :green :red :orange :yellow :purple])
(defonce piece-types [:square :straight :s1 :s2 :l1 :l2])

(defn generate-board []
  (vec (repeat board-height (vec (repeat board-width nil)))))

(defonce game-initial-state {:state :stopped
                             :active-piece-type nil
                             :active-piece-color nil
                             :board (generate-board)})


(defonce game (atom game-initial-state))

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

(defn xs-ys->update-actives! [xs-ys]
  (do (remove-actives!)
      (xs-ys->place-actives! xs-ys)))

(defn add-piece! []
  (let [color (get colors (random-up-to (count colors)))
        piece-type (get piece-types (random-up-to (count piece-types)))]
    (do (swap! game assoc :active-piece-color color)
        (swap! game assoc :active-piece-type piece-type)
        (cond
          (= piece-type :square)
          (xs-ys->place-actives! [[4 0] [5 0] [4 1] [5 1]])
          (= piece-type :straight)
          (xs-ys->place-actives! [[4 0] [4 1] [4 2] [4 2]])
          (= piece-type :s1)
          (xs-ys->place-actives! [[4 1] [5 1] [5 0] [6 0]])
          (= piece-type :s2)
          (xs-ys->place-actives! [[4 0] [5 0] [5 1] [6 1]])
          (= piece-type :l1)
          (xs-ys->place-actives! [[4 0] [4 1] [5 1] [6 1]])
          (= piece-type :l2)
          (xs-ys->place-actives! [[5 0] [5 1] [4 1] [3 1]])
          (= piece-type :t)
          (xs-ys->place-actives! [[4 0] [3 1] [4 1] [5 1]])))))

(defn rotate! []
  (let [{:keys [active-piece-type board]} @game
        new-xs-ys (board->rotated-active-xs-ys active-piece-type board)]
    (xs-ys->update-actives! new-xs-ys)))

(defn start! []
  (do (reset! game game-initial-state)
      (swap! game assoc :state :running)
      (add-piece!)))

(defn move-active-piece-down! []
  (xs-ys->update-actives! (board->shifted-down-active-xs-ys (:board @game))))

(defn move-left! []
  (xs-ys->update-actives! (board->shifted-left-active-xs-ys (:board @game))))

(defn move-right! []
  (xs-ys->update-actives! (board->shifted-right-active-xs-ys (:board @game))))

(defn deactivate-piece! []
  (let [deactivated-board (vec (map (fn [row]
                                      (vec (map (fn [sq]
                                                  (if (:active sq) (assoc sq :active false) sq))
                                                row)))
                                    (@game :board)))]
    (swap! game assoc :board deactivated-board)))

(defn tick! []
  (when (= (@game :state) :running)
    (if (piece-can-move-down? (@game :board))
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
              (cond is-space (when (piece-can-rotate? (:active-piece-type @game) (:board @game))
                               (rotate!))
                    is-down (tick!)
                    is-left (when (piece-can-move-left? (:board @game)) (move-left!))
                    is-right (when (piece-can-move-right? (:board @game)) (move-right!)))))]
    (create-class
     {:component-did-mount (fn [] (do
                                    (start!)
                                    (.addEventListener js/document "keydown" keyboard-listeners)
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
                                        :style {:grid-column (+ x 1) :grid-row (+ y 1)
                                                :background color}}
                                       [:span.piece-container]]))
                                  row))
                               board)]]
                            [:div.button-container
                             [:button {:on-click #(start!)} "start"]]]]))})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (reagent/render-component [tetris] el)))

;; https://figwheel.org/docs/hot_reloading.html#re-rendering-ui-after-saving-a-file
;; (defn ^:after-load re-render []
;;   (mount-app-element))
(defonce start-up (do (mount-app-element) true))
