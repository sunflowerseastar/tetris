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

(defn x-y-in-bounds? [x-y board]
  (let [x (first x-y) y (second x-y)
        x-in-bounds (and (>= x 0) (< x (count (first board))))
        y-in-bounds (and (>= y 0) (< y (count board)))]
    (and x-in-bounds y-in-bounds)))

(defn get-x-y [board x-y]
  (let [x (first x-y) y (second x-y)]
    (if (x-y-in-bounds? x-y board)
      (get-square x y board)
      nil)))

(defn xs-ys-in-bounds? [xs-ys board]
  (let [board-width (count (first board))
        board-height (count board)
        xs (map first xs-ys)
        ys (map second xs-ys)
        max-x (reduce max xs)
        min-x (reduce min xs)
        x-in-bounds (and (>= min-x 0) (< max-x board-width))
        max-y (reduce max ys)
        y-in-bounds (< max-y board-height)]
    (and x-in-bounds y-in-bounds)))

(defn xs-ys-are-free? [xs-ys board]
  (->> xs-ys
       (map #(get-x-y board %))
       (filter not-active-p)
       (map nil?)
       (every? true?)))

(defn some-square-below-are-non-empty-p [squares board]
  (->> squares
       (squares-below board)
       (filter not-active-p)
       (map nil?)
       (some false?)))

(defn some-square-left-are-non-empty-p [squares board]
  (->> squares
       (map (fn [square] (get-square (dec (:x square)) (:y square) board)))
       (filter #(not (= (:active %) true)))
       (map nil?)
       (some false?)))

(defn some-square-right-are-non-empty-p [squares board]
  (->> squares
       (map (fn [square] (get-square (inc (:x square)) (:y square) board)))
       (filter #(not (= (:active %) true)))
       (map nil?)
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

(defn board->rotated-active-xs-ys [piece-type board]
  (let [actives (get-actives board)
        xs (map :x actives)
        ys (map :y actives)]
    (cond (= piece-type :square) (map (fn [{:keys [x y]}] [x y]) actives)
          (= piece-type :straight)
          (let [is-vertical (apply = xs)]
            (if is-vertical
              (let [new-y (second ys)
                    new-xs (map #(+ (- (first xs) 1) %) [0 1 2 3])
                    new-xs-ys (map (fn [x] [x new-y]) new-xs)]
                new-xs-ys)
              (let [new-x (second xs)
                    starting-y (dec (first ys))
                    new-ys (range starting-y (+ starting-y 4))
                    new-xs-ys (map (fn [y] [new-x y]) new-ys)]
                new-xs-ys))))))

(defn piece-can-rotate-p [piece-type board]
  (let [new-xs-ys (board->rotated-active-xs-ys piece-type board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))

(defn board->shifted-right-active-xs-ys [board]
  (map (fn [{:keys [x y]}] [(inc x) y]) (get-actives board)))

(defn piece-can-move-right-p [board]
  (let [new-xs-ys (board->shifted-right-active-xs-ys board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))

(defn board->shifted-left-active-xs-ys [board]
  (map (fn [{:keys [x y]}] [(dec x) y]) (get-actives board)))

(defn piece-can-move-left-p [board]
  (let [new-xs-ys (board->shifted-left-active-xs-ys board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))
