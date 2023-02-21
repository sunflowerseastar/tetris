(ns tetris.components
  (:require [tetris.helpers :refer [piece-matrix->coords]]))

(defn board [board-width game board-y-negative-offset]
  [:div.board {:style {:gridTemplateColumns (str "repeat(" board-width ", 1fr)")}}
   (map-indexed
    (fn [y row]
      (map-indexed
       (fn [x square]
         (let [{:keys [color-hex]} square]
           [:div.square
            {:key (str x y)
             :class [(when (zero? x) "left-edge") (when (zero? y) "top-edge")]
             :style {:grid-column (+ x 1) :grid-row (+ y 1)
                     :background color-hex}}]))
       row))
    (drop board-y-negative-offset (:board @game)))])

(defn upcoming-piece-component [game bump-queue! tetrominoes]
  [:div.upcoming-piece
   {:on-click #(when (not (:game-over @game)) (bump-queue!))}
   (let [{:keys [color-hex piece-type]} (first (:piece-queue @game))
         piece-matrix (-> tetrominoes (get (keyword piece-type)) :piece-matrix-rotations first)
         coords (piece-matrix->coords piece-matrix [0 0])
         matrix-for-grid (vec (repeat (count piece-matrix)
                                      (vec (repeat (-> piece-matrix first count) 0))))]
     (map-indexed
      (fn [y row]
        (map-indexed
         (fn [x]
           (let [is-a-unit-square (some #{[x y]} coords)]
             [:div.upcoming-piece-square
              {:key (str x y)
               :style {:grid-column (+ x 1) :grid-row (+ y 1)
                       :background (when is-a-unit-square color-hex)}}]))
         row))
      matrix-for-grid))])

(defn rows-completed-component [game pause-or-unpause!]
  [:span.rows-completed
   {:class (when (:is-paused @game) "is-paused")
    :on-click #(when (not (:game-over @game)) (pause-or-unpause!))}
   (:rows-completed @game)])

(defn level-component [level]
  [:span.level level])
