(ns ^:figwheel-hooks tetris.core
  (:require
   [tetris.components :refer [board
                              level-component
                              rows-completed-component
                              upcoming-piece-component]]
   [tetris.helpers :refer [active-piece-game-state->active-piece-coords
                           board->board-without-actives
                           board->board-without-completions
                           board-has-4-in-a-row?
                           cycle-rotation-index
                           generate-board
                           piece-matrix->coords
                           piece-can-move-down?
                           piece-can-move-left?
                           piece-can-move-right?
                           piece-can-rotate?
                           rotate
                           shift-piece-matrix-down
                           shift-piece-matrix-left
                           shift-piece-matrix-right
                           coords-are-free?]]
   [goog.dom :as gdom]
   [reagent.dom :as rdom]
   [reagent.ratom :as reagent.ratom]
   [reagent.core :as reagent :refer [atom create-class]]))

;; TODO move hexs to css
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

;; TODO expand and wire to controls
(def user-options (atom {:is-helpful false}))

(def tetrominoes
  {:square {:piece-type :square
            :starting-y-offset 0
            :color-hex (:lavender colors)
            :piece-matrix-rotations [[[1 1] [1 1]]]}
   :straight {:piece-type :straight
              :starting-y-offset -2
              :starting-x-offset -1
              :color-hex (:orange colors)
              :piece-matrix-rotations [[[0 0 0 0] [0 0 0 0] [1 1 1 1] [0 0 0 0]]
                                       [[0 1 0 0] [0 1 0 0] [0 1 0 0] [0 1 0 0]]]}
   :s1 {:piece-type :s1
        :starting-y-offset -1
        :color-hex (:green colors)
        :piece-matrix-rotations [[[0 0 0] [0 1 1] [1 1 0]]
                                 [[1 0 0] [1 1 0] [0 1 0]]]}
   :s2 {:piece-type :s2
        :starting-y-offset -1
        :color-hex (:pink colors)
        :piece-matrix-rotations [[[0 0 0] [1 1 0] [0 1 1]]
                                 [[0 0 1] [0 1 1] [0 1 0]]]}
   :l1 {:piece-type :l1
        :starting-y-offset -1
        :color-hex (:red colors)
        :piece-matrix-rotations [[[0 0 0] [1 1 1] [0 0 1]]
                                 [[0 1 0] [0 1 0] [1 1 0]]
                                 [[1 0 0] [1 1 1] [0 0 0]]
                                 [[0 1 1] [0 1 0] [0 1 0]]]}
   :l2 {:piece-type :l2
        :starting-y-offset -1
        :color-hex (:blue colors)
        :piece-matrix-rotations [[[0 0 0] [1 1 1] [1 0 0]]
                                 [[1 1 0] [0 1 0] [0 1 0]]
                                 [[0 0 1] [1 1 1] [0 0 0]]
                                 [[0 1 0] [0 1 0] [0 1 1]]]}
   :t {:piece-type :t
       :starting-y-offset -1
       :color-hex (:yellow colors)
       :piece-matrix-rotations [[[0 0 0] [1 1 1] [0 1 0]]
                                [[0 1 0] [1 1 0] [0 1 0]]
                                [[0 1 0] [1 1 1] [0 0 0]]
                                [[0 1 0] [0 1 1] [0 1 0]]]}})

(defn generate-piece-queue [] (repeatedly queue-length #(-> tetrominoes shuffle first val)))

(def game-initial-state {:is-paused true
                         :active-piece-top-left-x 0
                         :active-piece-top-left-y 0
                         :active-piece-type nil
                         :active-piece-color nil
                         :active-rotation-index 0
                         :active-piece nil
                         :game-over true
                         :rows-completed 0
                         :piece-queue nil
                         :level 1
                         :bump-level false
                         :tick-duration 700
                         :board (generate-board board-width (+ board-height board-y-negative-offset))})

(def tick-interval (atom 0))
(def down-touch-interval (atom 0))
(def left-touch-interval (atom 0))
(def right-touch-interval (atom 0))

(def game (atom game-initial-state))

;; 'active-piece-coords' is the current active piece, translated from its matrix
;; form to xy coordinates on the board form
(def active-piece-coords (reagent.ratom/reaction (active-piece-game-state->active-piece-coords @game)))

;; TODO add an active-state cursor for active-piece-game-state->active-piece-coords

(defn bump-queue! []
  (let [;; give straight for a 4-in-a-row
        upcoming-piece (if (and (:is-helpful user-options) (board-has-4-in-a-row? (:board @game)))
                         (-> tetrominoes :straight val)
                         (-> tetrominoes shuffle first val))]
    (swap! game update :piece-queue #(->> % (drop 1) (cons upcoming-piece)))))

;; TODO refactor approach of this function and the following one - perform this
;; purely on the board, then swap out the board.
(defn coords->place-actives!
  "Given a list of xy coordinate pairs, update the game board in those locations. Make them active and give them the current active piece's color."
  [coords]
  (when (seq coords)
    (let [[x y] (first coords)]
      (swap! game assoc-in [:board y x] {:color-hex (:active-piece-color @game) :active true})
      (coords->place-actives! (rest coords)))))

(defn coords->replace-actives!
  "Given a list of xy coordinate pairs, clear the game board's current actives and
  update it with the provided xy coordinate locations as active."
  [coords]
  (swap! game update :board board->board-without-actives)
  (coords->place-actives! coords))

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

(defn add-piece!
  "Take the next piece in the queue and add it to the board by updating all the
  'active-piece' state."
  []
  (let [;; get the next piece the queue
        new-active-piece (first (:piece-queue @game))
        ;; TODO move color handling from top-level state to active-piece sub-state
        {:keys [color-hex piece-type starting-x-offset starting-y-offset piece-matrix-rotations]} new-active-piece
        centering-x-offset (-> board-width (quot 2) (- 1))
        starting-top-left-x (+ centering-x-offset starting-x-offset)
        starting-top-left-y (+ starting-y-offset board-y-negative-offset)
        starting-top-left-xy [starting-top-left-x starting-top-left-y]
        new-active-piece-coords (piece-matrix->coords (first piece-matrix-rotations) starting-top-left-xy)
        new-piece-overlaps-existing-squares (not (coords-are-free? new-active-piece-coords (:board @game)))]
    ;; (println "add-piece!")
    ;; remove the piece-being-added from front of the queue
    (bump-queue!)
    ;; update all the 'active piece' state
    (swap! game assoc :active-piece-color color-hex)
    (swap! game assoc :active-piece-type piece-type)
    (swap! game assoc :active-piece-top-left-x starting-top-left-x)
    (swap! game assoc :active-piece-top-left-y starting-top-left-y)
    (swap! game assoc :active-rotation-index 0)
    (swap! game assoc :active-piece new-active-piece)
    (coords->place-actives! new-active-piece-coords)
    (when new-piece-overlaps-existing-squares (end-game!))))

(defn rotate! []
  (let [new-rotation-index (cycle-rotation-index
                            (:active-rotation-index @game)
                            (get-in @game [:active-piece :piece-matrix-rotations]))
        rotated-active-coords (rotate @game)]
    (swap! game assoc :active-rotation-index new-rotation-index)
    (coords->replace-actives! rotated-active-coords)))

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
  (coords->replace-actives! (shift-piece-matrix-down @active-piece-coords))
  (swap! game update :active-piece-top-left-y inc))

(defn move-left! []
  (coords->replace-actives! (shift-piece-matrix-left @active-piece-coords))
  (swap! game update :active-piece-top-left-x dec))

(defn move-right! []
  (coords->replace-actives! (shift-piece-matrix-right @active-piece-coords))
  (swap! game update :active-piece-top-left-x inc))

(defn tick! []
  (when (not (:game-over @game))
    (if (piece-can-move-down? @active-piece-coords (:board @game))
      (move-active-piece-down!)
      (do (deactivate-piece!)
          (handle-completed-rows!)
          (when (:bump-level @game)
            (js/clearInterval @tick-interval)
            (reset! tick-interval (js/setInterval #(tick!) (:tick-duration @game)))
            (swap! game assoc :bump-level false))
          ;; (println "tick! -> add-piece!")
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
                                                          (piece-can-rotate? @game))
                                                 (rotate!)))
                    is-up (when (and is-running (piece-can-rotate? @game)) (rotate!))
                    is-p (when (not is-game-over) (pause-or-unpause!))
                    is-n (when (not is-game-over) (bump-queue!))
                    ;; FIXME when piece is at the bottom, user cannot press
                    ;; 'down' and go ahead and start with the next piece. User
                    ;; has to wait the normal tick time duration to get the next
                    ;; add-piece
                    (or is-d is-down) (when is-running (tick!))
                    is-left (when (and is-running (piece-can-move-left? @active-piece-coords (:board @game))) (move-left!))
                    is-right (when (and is-running (piece-can-move-right? @active-piece-coords (:board @game))) (move-right!))
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
             [:div.hit-area-up {:on-click #(when (and is-running (piece-can-rotate? @game)) (rotate!))}]]
            [:div.hit-area-row
             [:div.hit-area-left
              {:on-touch-start #(reset! left-touch-interval
                                        (do
                                          (when (and is-running (piece-can-move-left? @active-piece-coords (:board @game))) (move-left!))
                                          ;; TODO improve the hold-and-move timing (make it wait for a sec, then start going - a bit faster than prev setting)
                                          (js/setInterval (fn [] (when (and is-running (piece-can-move-left? @active-piece-coords (:board @game))) (move-left!))) 150)))
               :on-touch-end #(js/clearInterval @left-touch-interval)}]
             [:div.hit-area-down
              {:on-touch-start #(reset! down-touch-interval (js/setInterval (fn [] (tick!)) 50))
               :on-touch-end #(js/clearInterval @down-touch-interval)}]
             [:div.hit-area-right
              {:on-touch-start #(reset! right-touch-interval
                                        (do
                                          (when (and is-running (piece-can-move-right? @active-piece-coords (:board @game))) (move-right!))
                                          (js/setInterval (fn [] (when (and is-running (piece-can-move-right? @active-piece-coords (:board @game))) (move-right!))) 150)))
               :on-touch-end #(js/clearInterval @right-touch-interval)}]]])

         ;; gameplay area
         [:div.gameplay-container
          [:div.board-container {:class [(when (:is-paused @game) "is-paused")
                                         (when (:game-over @game) "game-over")]}
           [board board-width game board-y-negative-offset]]
          [:div.meta-container
           [:div.upcoming-piece-container {:class (when (:is-paused @game) "is-paused")}
            [upcoming-piece-component game bump-queue! tetrominoes]]
           [:div.rows-completed-container
            [rows-completed-component game bump-queue!]]
           [:div.level-container {:class (when (:is-paused @game) "is-paused")}
            [level-component (:level @game)]]]]])})))

(defn mount-app-element []
  (when-let [el (gdom/getElement "app")]
    (rdom/render [tetris] el)))

(defonce start-up (do (mount-app-element) true)
  )
