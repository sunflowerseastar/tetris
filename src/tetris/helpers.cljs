(ns tetris.helpers
  (:require
   [tupelo.core :refer [it-> spyx]]))

(defn random-up-to [n]
  (js/parseInt (* (.random js/Math) n)))

(defn get-square [x y board]
  (-> board (nth y) (nth x)))

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

(defn get-actives [board]
  (->> board
       flatten
       (filter #(= (:active %) true))))

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
       (filter #(not (= (:active %) true)))
       (map nil?)
       (every? true?)))

(defn board->shift [x-fn y-fn board]
  (map (fn [{:keys [x y]}] [(x-fn x) (y-fn y)]) (get-actives board)))

(defn board->shifted-down-active-xs-ys [board]
  (board->shift identity inc board))

(defn board->shifted-left-active-xs-ys [board]
  (board->shift dec identity board))

(defn board->shifted-right-active-xs-ys [board]
  (board->shift inc identity board))

(defn piece-can-move-p [shift-fn board]
  (let [new-xs-ys (shift-fn board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))

(defn piece-can-move-down-p [board]
  (piece-can-move-p board->shifted-down-active-xs-ys board))

(defn piece-can-move-left-p [board]
  (piece-can-move-p board->shifted-left-active-xs-ys board))

(defn piece-can-move-right-p [board]
  (piece-can-move-p board->shifted-right-active-xs-ys board))

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
                new-xs-ys)))
          (= piece-type :l1)
          (let [max-x (reduce max xs)
                is-pos-1 (apply = [(second ys) (nth ys 2) (nth ys 3)])
                is-pos-2 (= (count (filter #(= % max-x) xs)) 3)
                is-pos-3 (apply = [(first ys) (second ys) (nth ys 2)])]
            (cond is-pos-1
                  (let [block-1 (it-> actives (second it) [(:x it) (:y it)])
                        block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                        block-3 (it-> actives (first it) [(inc (:x it)) (:y it)])
                        block-4 (it-> actives (first it) [(inc (:x it)) (dec (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-2
                  (let [block-1 (it-> actives (second it) [(:x it) (:y it)])
                        block-2 (it-> actives (nth it 3) [(:x it) (:y it)])
                        block-3 (it-> actives (nth it 2) [(:x it) (dec (:y it))])
                        block-4 (it-> actives (nth it 2) [(dec (:x it)) (dec (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-3
                  (let [block-1 (it-> actives (second it) [(:x it) (:y it)])
                        block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                        block-3 (it-> actives (nth it 3) [(dec (:x it)) (:y it)])
                        block-4 (it-> actives (nth it 3) [(dec (:x it)) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  :else
                  (let [block-1 (it-> actives (first it) [(:x it) (:y it)])
                        block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                        block-3 (it-> actives (second it) [(:x it) (inc (:y it))])
                        block-4 (it-> actives (second it) [(inc (:x it)) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys))))))

(defn piece-can-rotate-p [piece-type board]
  (let [new-xs-ys (board->rotated-active-xs-ys piece-type board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))
