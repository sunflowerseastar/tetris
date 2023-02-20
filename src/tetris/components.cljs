(ns tetris.components)

(defn board [board-width game board-y-negative-offset]
  [:div.board {:style {:gridTemplateColumns (str "repeat(" board-width ", 1fr)")}}
   (map-indexed
    (fn [y row]
      (map-indexed
       (fn [x square]
         (let [{:keys [color]} square]
           [:div.square
            {:key (str x y)
             :class [(when (zero? x) "left-edge") (when (zero? y) "top-edge")]
             :style {:grid-column (+ x 1) :grid-row (+ y 1)
                     :background color}}]))
       row))
    (drop board-y-negative-offset (:board @game)))])

(defn upcoming-piece-component [game bump-queue! base-pieces]
  [:div.upcoming-piece
   {:on-click #(when (not (:game-over @game)) (bump-queue!))}
   (let [upcoming-piece (first (:piece-queue @game))
         {:keys [color-rgb-hex piece-type]} upcoming-piece
         base-xs-ys (->> base-pieces
                         (filter #(= (:piece-type %) piece-type))
                         first
                         :xs-ys)
         xs (map first base-xs-ys) ys (map second base-xs-ys)
         min-x (reduce min xs) max-x (reduce max xs) width-x (inc (- max-x min-x))
         min-y (reduce min ys) max-y (reduce max ys) height-y (inc (- max-y min-y))
         matrix-for-grid (vec (repeat height-y (vec (repeat width-x 0))))]
     (map-indexed
      (fn [y row]
        (map-indexed
         (fn [x]
           (let [match (some #{[x y]} base-xs-ys)]
             [:div.upcoming-piece-square {:key (str x y)
                                          :style {:grid-column (+ x 1) :grid-row (+ y 1)
                                                  :background (when match color-rgb-hex)}}]))
         row))
      matrix-for-grid))])

(defn rows-completed-component [game pause-or-unpause!]
  [:span.rows-completed
   {:class (when (:is-paused @game) "is-paused")
    :on-click #(when (not (:game-over @game)) (pause-or-unpause!))}
   (:rows-completed @game)])

(defn level-component [level]
  [:span.level level])
