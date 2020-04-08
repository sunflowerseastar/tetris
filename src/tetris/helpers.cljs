(ns tetris.helpers
  (:require
   [tupelo.core :refer [spyx]]))

(defn get-square [x y board]
  (-> board (nth y) (nth x)))

(defn random-up-to [n]
  (js/parseInt (* (.random js/Math) n)))

(defn square-below [square board]
  (get-square (:x square) (inc (:y square)) board))

(defn squares-below [board squares]
  (map #(square-below % board) squares))

(defn not-active-p [square]
  (not (= (:active square) true)))

(defn some-square-below-are-non-empty-p [squares board]
  (->> squares
       (squares-below board)
       (filter not-active-p)
       (map empty?)
       (some false?)))

(defn some-square-left-are-non-empty-p [squares board]
  (->> squares
       (map (fn [square] (get-square (dec (:x square)) (:y square) board)))
       (filter #(not (= (:active %) true)))
       (map empty?)
       (some false?)))

(defn some-square-right-are-non-empty-p [squares board]
  (->> squares
       (map (fn [square] (get-square (inc (:x square)) (:y square) board)))
       (filter #(not (= (:active %) true)))
       (map empty?)
       (some false?)))

(defn get-actives [board]
  (->> board
       flatten
       (filter #(= (:active %) true))))

(defn piece-can-move-down-p [board board-height]
  (let [actives (get-actives board)
        max-y (->> actives (map :y) (reduce max))
        has-reached-bottom-p (>= (inc max-y) board-height)]
    (and (not has-reached-bottom-p) (not (some-square-below-are-non-empty-p actives board)))))

(defn piece-can-move-left-p [board]
  (let [actives (get-actives board)
        min-x (->> actives (map :x) (reduce min))
        has-reached-left-edge (< min-x 0)]
    (and (not has-reached-left-edge) (not (some-square-left-are-non-empty-p actives board)))))

(defn piece-can-move-right-p [board board-width]
  (let [actives (get-actives board)
        max-x (->> actives (map :x) (reduce max))
        has-reached-right-edge (> max-x board-width)]
    (and (not has-reached-right-edge) (not (some-square-right-are-non-empty-p actives board)))))
