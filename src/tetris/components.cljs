(ns tetris.components
  (:require
   [clojure.core.matrix :refer [new-matrix]]))

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
         matrix-for-grid (new-matrix height-y width-x)]
     (map-indexed
      (fn [y row]
        (map-indexed
         (fn [x]
           (let [match (some #{[x y]} base-xs-ys)]
             [:div {:key (str x y)
                    :style {:grid-column (+ x 1) :grid-row (+ y 1)
                            :background (when match color-rgb-hex)}}]))
         row))
      matrix-for-grid))])

(defn rows-completed-component [game pause-or-unpause! gradient-pairs]
  [:span.rows-completed
   {:class (when (:is-paused @game) "is-paused")
    :on-click #(when (not (:game-over @game)) (pause-or-unpause!))
    :style {:background (str "-webkit-linear-gradient(45deg, "
                             (-> (first (first gradient-pairs)) val) ", "
                             (-> (second (first gradient-pairs)) val) " 80%)")
            :backgroundClip "border-box"
            :-webkitBackgroundClip "text"
            :-webkitTextFillColor "transparent"}}
   (:rows-completed @game)])

(defn level-component [level gradient-pairs]
  [:<>
   [:div.level-mobile
    (map-indexed
     (fn [i gradient-pair]
       [:span.level
        {:key (str i (-> (first gradient-pair) val))
         :class (if (<= i (rem level (count gradient-pairs))) "in")
         :style {:color (-> (first gradient-pair) val)}}
        level])
     gradient-pairs)]
   [:div.level-desktop
    (map-indexed
     (fn [i gradient-pair]
       [:span.level
        {:key (str i (-> (first gradient-pair) val))
         :class (if (<= i (rem level (count gradient-pairs))) "in")
         :style {:background (str "-webkit-linear-gradient(45deg, "
                                  (-> (first gradient-pair) val) ", "
                                  (-> (second gradient-pair) val) " 80%)")
                 :backgroundClip "border-box"
                 :-webkitBackgroundClip "text"
                 :-webkitTextFillColor "transparent"}}
        level])
     gradient-pairs)]])
