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
                           piece-can-move-down-2?
                           piece-can-move-left?
                           piece-can-move-right?
                           piece-can-rotate?
                           xs-ys-are-free?]]
   [goog.dom :as gdom]
   [reagent.dom :as rdom]
   [reagent.core :as reagent :refer [atom create-class]]))

(def colors {:lavender "#d0d0ff"
             :orange "#ffd3ad"
             :green "#b1e597"
             :pink "#ffbad1"
             :red "#ff8c94"
             :blue "#91cdf2"
             :yellow "#faedb9"})

(def rows-per-level-up 10)

(def has-initially-loaded (atom false))

(def board-width 10)
(def board-y-negative-offset 2)
(def board-height 20)
(def queue-length 1)
(def tick-duration-multiplier 0.9)
;; TODO rename pieces to tetronimos?
(def base-pieces [{:piece-type :square
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

;; TODO finish basic rotations and offsets
;; figure out their top left coordinate at the start...
;; ...and translate the rotation (zero at start) to x-ys
(def base-pieces-2
  [{:piece-type :square
    :starting-y-offset 0
    :color-rgb-hex (:lavender colors)
    :rotation 0
    :xs-ys-rotations [[[1 1] [1 1]]]}
   {:piece-type :straight
    :starting-y-offset -2
    :color-rgb-hex (:orange colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0 0] [0 0 0 0] [1 1 1 1] [0 0 0 0]]
                      [[0 1 0 0] [0 1 0 0] [0 1 0 0] [0 1 0 0]]]}
   {:piece-type :s1
    :starting-y-offset -1
    :color-rgb-hex (:green colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0] [0 1 1] [1 1 0]]
                      [[1 0 0] [1 1 0] [0 1 0]]]}
   {:piece-type :s2
    :starting-y-offset -1
    :color-rgb-hex (:pink colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0] [1 1 0] [0 1 1]]
                      [[0 0 1] [0 1 1] [0 1 0]]]}
   {:piece-type :l1
    :starting-y-offset -1
    :color-rgb-hex (:red colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0] [1 1 1] [0 0 1]]
                      [[0 1 0] [0 1 0] [1 1 0]]
                      [[1 0 0] [1 1 1] [0 0 0]]
                      [[0 1 1] [0 1 0] [0 1 0]]]}
   {:piece-type :l2
    :starting-y-offset -1
    :color-rgb-hex (:blue colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0] [1 1 1] [1 0 0]]
                      [[1 1 0] [0 1 0] [0 1 0]]
                      [[0 0 1] [1 1 1] [0 0 0]]
                      [[0 1 0] [0 1 0] [0 1 1]]]}
   {:piece-type :t
    :starting-y-offset -1
    :color-rgb-hex (:yellow colors)
    :rotation 0
    :xs-ys-rotations [[[0 0 0] [1 1 1] [0 1 0]]
                      [[0 1 0] [1 1 0] [0 1 0]]
                      [[0 1 0] [1 1 1] [0 0 0]]
                      [[0 1 0] [0 1 1] [0 1 0]]]}])

(defn pieces->offset-pieces [pieces board-width]
  (let [x-offset (-> board-width (quot 2) (- 1))
        static-y-offset board-y-negative-offset]
    (letfn [(add-offsets [xs-ys x-offset-adjusted]
              (map (fn [[x y]] [(+ x-offset-adjusted x) (+ static-y-offset y)]) xs-ys))]
      (mapv #(if (= (:piece-type %) :straight)
               (update % :xs-ys add-offsets (dec x-offset))
               (update % :xs-ys add-offsets x-offset))
            pieces))))

(def offset-pieces (pieces->offset-pieces base-pieces board-width))

(defn generate-piece-queue []
  ;; (repeatedly queue-length #(rand-nth offset-pieces))
  (repeatedly queue-length #(first offset-pieces))
  )

(defn generate-piece-queue-2 []
  ;; (repeatedly queue-length #(rand-nth offset-pieces))
  (repeatedly queue-length #(first base-pieces-2))
  )

(def game-initial-state {:is-paused true
                         :active-piece-top-left-x 0
                         :active-piece-top-left-y 0
                         :active-piece-type nil
                         :active-piece-color nil
                         :active-piece-rotation 0
                         :active-piece nil
                         :game-over true
                         :rows-completed 0
                         :piece-queue nil
                         :piece-queue-2 nil
                         :level 1
                         :bump-level false
                         :tick-duration 700
                         :board (generate-board board-width (+ board-height board-y-negative-offset))})

(def tick-interval (atom 0))
(def down-touch-interval (atom 0))
(def left-touch-interval (atom 0))
(def right-touch-interval (atom 0))

(def game (atom game-initial-state))

;; (def active-piece-cursor (reagent/cursor game [:active-piece]))
;; (def active-piece-cursor (reagent/ game [:active-piece]))

(defn bump-queue! []
  (let [upcoming-piece
        ;; give straight for a 4-in-a-row
        ;; (if (board-has-4-in-a-row? (:board @game))
        ;;   (get offset-pieces 1)
        ;;   (rand-nth offset-pieces))
        (first offset-pieces)]
    ;; (swap! game update :piece-queue #(as-> % it (drop 1 it) (append it upcoming-piece)))
    (swap! game update :piece-queue #(as-> % it (drop 1 it) (concat it upcoming-piece)))
    ))

(defn xs-ys->place-actives! [xs-ys]
  (when (seq xs-ys)
    (let [x-y (first xs-ys) x (first x-y) y (second x-y)]
      (swap! game assoc-in [:board y x] {:color (:active-piece-color @game) :active true :x x :y y})
      (xs-ys->place-actives! (rest xs-ys)))))

(defn remove-actives! []
  (swap! game update :board board->board-without-actives))

(defn xs-ys->replace-actives!
  [xs-ys]
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

(defn matrix->xs-ys
  "Translate a 'piece matrix' to the xy coordinates on the board.
  Ex. [[1 1] [1 1]] -> [[0 0] [1 0] [0 1] [1 1]]"
  [piece-matrix]
  (let [unfiltered-xs-ys (map-indexed
                          (fn [y row]
                            (map-indexed
                             (fn [x val] (when (pos? val) [y x]))
                             row))
                          piece-matrix)
        filtered-xs-ys (map #(filter identity %) unfiltered-xs-ys)]
    (->> filtered-xs-ys
         (apply concat)
         vec)))

(defn matrix-and-offsets->xs-ys
  "Translate a 'piece matrix' to the xy coordinates on the board.
  Ex. [[1 1] [1 1]] -> [[0 0] [1 0] [0 1] [1 1]]"
  ([piece-matrix]
   (matrix-and-offsets->xs-ys piece-matrix 0 0))
  ([piece-matrix starting-y-offset centering-x-offset]
   (let [unfiltered-xs-ys (map-indexed
                           (fn [y row]
                             (map-indexed
                              (fn [x val]
                                (when (pos? val)
                                  [(+ x centering-x-offset)
                                   (+ y starting-y-offset board-y-negative-offset)]))
                              row))
                           piece-matrix)
         filtered-xs-ys (map #(filter identity %) unfiltered-xs-ys)]
     (->> filtered-xs-ys
          (apply concat)
          vec))))

(defn m2
  "Given a 'piece matrix' and an active piece 'top-left' [x y], return all the corresponding xy coordinates on the board.
  Ex. [[1 1] [1 1]] [0 0] -> [[0 0] [1 0] [0 1] [1 1]]"
  ([piece-matrix [active-piece-top-left-x active-piece-top-left-y]]
   (let [unfiltered-xs-ys (map-indexed
                           (fn [y row]
                             (map-indexed
                              (fn [x val]
                                (when (pos? val)
                                  [(+ x active-piece-top-left-x)
                                   (+ y active-piece-top-left-y)]))
                              row))
                           piece-matrix)
         filtered-xs-ys (map #(filter identity %) unfiltered-xs-ys)]
     (->> filtered-xs-ys
          (apply concat)
          vec))))

(defn add-piece!
  "Take the next piece in the queue and add it to the board by updating all the
  'active-piece' state."
  []
  (let [;; get the next piece the queue
        ;; {:keys [xs-ys]} (first (:piece-queue @game))
        new-active-piece (first (:piece-queue-2 @game))
        {:keys [color-rgb-hex piece-type starting-y-offset xs-ys-rotations]} new-active-piece
        centering-x-offset (-> board-width (quot 2) (- 1))
        starting-top-left-xy [centering-x-offset
                              (+ starting-y-offset board-y-negative-offset)]
        xs-ys-4 (m2 (first xs-ys-rotations) starting-top-left-xy)
        new-piece-overlaps-existing-squares (not (xs-ys-are-free? xs-ys-4 (:board @game)))]
    (println "add-piece!")
    ;; (println (first (:piece-queue @game)))
    ;; (println xs-ys)
    ;; (println new-active-piece)
    ;; (println xs-ys-2)
    ;; (println centering-x-offset)
    ;; (println xs-ys-3)
    (println xs-ys-4)
    ;; remove the piece-being-added from front of the queue
    (bump-queue!)
    ;; update all the 'active piece' state
    (swap! game assoc :active-piece-color color-rgb-hex)
    (swap! game assoc :active-piece-type piece-type)
    (swap! game assoc :active-piece-top-left-x centering-x-offset)
    (swap! game assoc :active-piece-top-left-y (+ starting-y-offset board-y-negative-offset))
    (swap! game assoc :active-rotation 0)
    (swap! game assoc :active-piece new-active-piece)
    (xs-ys->place-actives! xs-ys-4)
    (when new-piece-overlaps-existing-squares (end-game!))))

(defn rotate! []
  (let [{:keys [active-piece-type board]} @game
        new-xs-ys (board->rotated-active-xys active-piece-type board)]
    ;; TODO old to new piece types
    (xs-ys->replace-actives! new-xs-ys)))

(defn deactivate-piece!
  "Swap out the board with one where all squares are {:active false}."
  []
  (let [deactivated-board
        (mapv (fn [row]
                (mapv (fn [sq]
                        (if (:active sq)
                          (assoc sq :active false)
                          sq))
                      row))
              (:board @game))]
    (swap! game assoc :board deactivated-board)))

(defn move-active-piece-down! []
  ;; TODO old to new piece types
  (let [{:keys [active-piece active-piece-top-left-x active-piece-top-left-y active-piece-rotation]} @game
        xys-2 (m2 (nth (:xs-ys-rotations active-piece) active-piece-rotation) [active-piece-top-left-x active-piece-top-left-y])]
    (swap! game update :active-piece-top-left-y inc)
    (println xys-2)
    ;; (xs-ys->replace-actives! (board->shifted-down-active-xys (:board @game)))
    (xs-ys->replace-actives! xys-2)))

(defn move-left! []
  ;; TODO old to new piece types
  (let [{:keys [active-piece active-piece-top-left-x active-piece-top-left-y active-piece-rotation]} @game
        xys-2 (m2 (nth (:xs-ys-rotations active-piece) active-piece-rotation) [active-piece-top-left-x active-piece-top-left-y])]
    ;; (xs-ys->replace-actives! (board->shifted-left-active-xys (:board @game)))
    (swap! game update :active-piece-top-left-x dec)
    (println xys-2)
    (xs-ys->replace-actives! xys-2)
    ))

(defn move-right! []
  ;; TODO old to new piece types
  (xs-ys->replace-actives!
   (board->shifted-right-active-xys (:board @game))))

(defn tick! []
  (when (not (:game-over @game))
    (if (piece-can-move-down-2? (:board @game))
      (move-active-piece-down!)
      (do (deactivate-piece!)
          (handle-completed-rows!)
          (when (:bump-level @game)
            (js/clearInterval @tick-interval)
            (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game)))
            (swap! game assoc :bump-level false))
          (println "tick! -> add-piece!")
          (add-piece!)))
    #_(if (piece-can-move-down? (:board @game))
        (move-active-piece-down!)
        (do (deactivate-piece!)
            (handle-completed-rows!)
            (when (:bump-level @game)
              (js/clearInterval @tick-interval)
              (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game)))
              (swap! game assoc :bump-level false))
            (println "tick! -> add-piece!")
            (add-piece!)))
    ))

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
  (swap! game assoc :piece-queue-2 (generate-piece-queue-2))
  (swap! game assoc :game-over false)
  (println "start-game! -> add-piece!")
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
                    ;; FIXME when piece is at the bottom, user cannot press
                    ;; 'down' and go ahead and start with the next piece. User
                    ;; has to wait the normal tick time duration to get the next
                    ;; add-piece
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
    (rdom/render [tetris] el)))

(defonce start-up (do (mount-app-element) true))
