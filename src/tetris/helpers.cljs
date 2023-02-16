(ns tetris.helpers
  (:require
   [tupelo.core :refer [it-> not-nil? prepend spyx]]))

(defn generate-blank-row [width]
  (vec (repeat width nil)))

(defn generate-board [board-width board-height]
  (vec (repeat board-height (generate-blank-row board-width))))

(defn board->board-without-completions [board board-width]
  (let [blank-row (generate-blank-row board-width)]
    (loop [rows board new-board [] num-rows-completed 0]
      (if (empty? rows) [new-board num-rows-completed]
          (let [row (first rows)
                is-completed (->> row (map not-nil?) (every? true?))]
            (if is-completed
              (recur (rest rows) (prepend blank-row new-board) (inc num-rows-completed))
              (recur (rest rows) (conj new-board row) num-rows-completed)))))))

(defn board->board-without-actives [board]
  (mapv (fn [row] (mapv #(if (true? (:active %)) nil %) row)) board))

(defn x-y-in-bounds? [x-y board]
  (let [x (first x-y) y (second x-y)
        x-in-bounds (and (>= x 0) (< x (count (first board))))
        y-in-bounds (and (>= y 0) (< y (count board)))]
    (and x-in-bounds y-in-bounds)))

(defn get-square [board x-y]
  (if (x-y-in-bounds? x-y board)
    (-> board (nth (second x-y)) (nth (first x-y)))
    nil))

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
        min-y (reduce min ys)
        max-y (reduce max ys)
        y-in-bounds (and (>= min-y 0) (< max-y board-height))]
    (and x-in-bounds y-in-bounds)))

(defn xs-ys-are-free?
  "Given the shifted xy's of an active piece and a board, return boolean of
  whether all of those squares are unoccupied."
  [shifted-active-xys board]
  (->> shifted-active-xys
       ;; get the current board squares of the shifted xy's
       (map #(get-square board %))
       ;; ignore the active pieces, so it'll either be nil or an inactive piece
       (filter #(not (true? (:active %))))
       ;; return whether all these squares are empty
       (every? nil?)))

(defn board->shift
  "Given an shift function for x, an shift function for y, and a board,
  return a board with the x's and y's of all the active pieces adjusted. For
  example, to move all the active pieces to the left, the x-fn would be dec and
  the y-fn would be identity."
  [x-fn y-fn board]
  (map (fn [{:keys [x y]}] [(x-fn x) (y-fn y)]) (get-actives board)))

(defn board->shifted-down-active-xys
  "Given a board, return the xy's of the active piece shifted down one square. The
  following two functions do the same thing for shifting left & right."
  [board]
  (board->shift identity inc board))

(defn board->shifted-left-active-xys [board]
  (board->shift dec identity board))

(defn board->shifted-right-active-xys [board]
  (board->shift inc identity board))

(defn are-shifted-active-xys-in-bounds-and-free?
  "Given the xy's of a shifted active piece and a board, return a boolean whether
  it's a legal shift or not. The following three functions use this to see if
  the active piece can shift down, left, and right."
  [shifted-active-xys board]
  (and (xs-ys-in-bounds? shifted-active-xys board)
       (xs-ys-are-free? shifted-active-xys board)))

(defn piece-can-move-down? [board]
  (are-shifted-active-xys-in-bounds-and-free?
   (board->shifted-down-active-xys board) board))

(defn piece-can-move-left? [board]
  (are-shifted-active-xys-in-bounds-and-free?
   (board->shifted-left-active-xys board) board))

(defn piece-can-move-right? [board]
  (are-shifted-active-xys-in-bounds-and-free?
   (board->shifted-right-active-xys board) board))

;; TODO rewrite - this is terrible
(defn board->rotated-active-xys [piece-type board]
  (let [actives (get-actives board)
        xs (map :x actives)
        ys (map :y actives)]
    (cond (= piece-type :square) (map (fn [{:keys [x y]}] [x y]) actives)
          (= piece-type :straight)
          (let [is-vertical (apply = xs)]
            (if is-vertical
              (let [new-y (nth ys 2)
                    new-xs (map #(+ (- (first xs) 1) %) [0 1 2 3])
                    new-xs-ys (map (fn [x] [x new-y]) new-xs)]
                new-xs-ys)
              (let [new-x (second xs)
                    starting-y (- (first ys) 2)
                    new-ys (range starting-y (+ starting-y 4))
                    new-xs-ys (map (fn [y] [new-x y]) new-ys)]
                new-xs-ys)))
          (= piece-type :s1)
          (let [min-x (reduce min xs)
                max-x (reduce max xs)
                delta-x (- max-x min-x)
                is-pos-1 (= delta-x 2)]
            (if is-pos-1
              (let [block-1 (it-> actives (first it) [(:x it) (:y it)])
                    block-2 (it-> actives (nth it 3) [(:x it) (:y it)])
                    block-3 (it-> actives (first it) [(dec (:x it)) (:y it)])
                    block-4 (it-> actives (first it) [(dec (:x it)) (dec (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              (let [
                    block-1 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-2 (it-> actives (nth it 2) [(inc (:x it)) (:y it)])
                    block-3 (it-> actives (nth it 3) [(dec (:x it)) (:y it)])
                    block-4 (it-> actives (nth it 3) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :s2)
          (let [max-x (reduce max xs)
                min-x (reduce min xs)
                delta-x (- max-x min-x)
                is-pos-1 (= delta-x 2)]
            (if is-pos-1
              (let [
                    block-1 (it-> actives (second it) [(inc (:x it)) (dec (:y it))])
                    block-2 (it-> actives (second it) [(:x it) (:y it)])
                    block-3 (it-> actives (second it) [(inc (:x it)) (:y it)])
                    block-4 (it-> actives (nth it 2) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              (let [
                    block-1 (it-> actives (second it) [(dec (:x it)) (:y it)])
                    block-2 (it-> actives (second it) [(:x it) (:y it)])
                    block-3 (it-> actives (nth it 3) [(:x it) (:y it)])
                    block-4 (it-> actives (nth it 3) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :l1)
          (let [max-x (reduce max xs)
                is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (= (count (filter #(= % max-x) xs)) 3)
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond
              is-pos-1
              (let [block-1 (it-> actives (second it) [(:x it) (dec (:y it))])
                    block-2 (it-> actives (second it) [(:x it) (:y it)])
                    block-3 (it-> actives (second it) [(:x it) (inc (:y it))])
                    block-4 (it-> actives (first it) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-2
              (let [block-1 (it-> actives (first it) [(dec (:x it)) (:y it)])
                    block-2 (it-> actives (second it) [(dec (:x it)) (:y it)])
                    block-3 (it-> actives (second it) [(:x it) (:y it)])
                    block-4 (it-> actives (second it) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-3
              (let [block-1 (it-> actives (first it) [(inc (:x it)) (:y it)])
                    block-2 (it-> actives (first it) [(+ 2 (:x it)) (:y it)])
                    block-3 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-4 (it-> actives (nth it 2) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              :else
              (let [block-1 (it-> actives (nth it 2) [(dec (:x it)) (:y it)])
                    block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-3 (it-> actives (nth it 2) [(inc (:x it)) (:y it)])
                    block-4 (it-> actives (nth it 3) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :l2)
          (let [max-x (reduce max xs)
                is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (= (count (filter #(= % max-x) xs)) 3)
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond is-pos-1
                  (let [block-1 (it-> actives (first it) [(:x it) (dec (:y it))])
                        block-2 (it-> actives (second it) [(:x it) (dec (:y it))])
                        block-3 (it-> actives (second it) [(:x it) (:y it)])
                        block-4 (it-> actives (second it) [(:x it) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-2
                  (let [block-1 (it-> actives (first it) [(:x it) (inc (:y it))])
                        block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                        block-3 (it-> actives (nth it 2) [(inc (:x it)) (:y it)])
                        block-4 (it-> actives (second it) [(inc (:x it)) (:y it)])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-3
                  (let [block-1 (it-> actives (first it) [(dec (:x it)) (:y it)])
                        block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                        block-3 (it-> actives (nth it 2) [(:x it) (inc (:y it))])
                        block-4 (it-> actives (nth it 3) [(:x it) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  :else
                  (let [block-1 (it-> actives (second it) [(dec (:x it)) (:y it)])
                        block-2 (it-> actives (second it) [(:x it) (:y it)])
                        block-3 (it-> actives (second it) [(inc (:x it)) (:y it)])
                        block-4 (it-> actives (nth it 2) [(dec (:x it)) (:y it)])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)))
          (= piece-type :t)
          (let [is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (apply = [(first xs) (second xs) (nth xs 3)])
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond
              is-pos-1
              (let [block-1 (it-> actives (second it) [(:x it) (dec (:y it))])
                    block-2 (it-> actives (first it) [(:x it) (:y it)])
                    block-3 (it-> actives (second it) [(:x it) (:y it)])
                    block-4 (it-> actives (nth it 3) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-2
              (let [block-1 (it-> actives (second it) [(dec (:x it)) (:y it)])
                    block-2 (it-> actives (second it) [(:x it) (:y it)])
                    block-3 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-4 (it-> actives (second it) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-3
              (let [
                    block-1 (it-> actives (first it) [(:x it) (:y it)])
                    block-2 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-3 (it-> actives (nth it 3) [(:x it) (:y it)])
                    block-4 (it-> actives (nth it 2) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              :else
              (let [block-1 (it-> actives (first it) [(:x it) (:y it)])
                    block-2 (it-> actives (second it) [(:x it) (:y it)])
                    block-3 (it-> actives (nth it 2) [(:x it) (:y it)])
                    block-4 (it-> actives (nth it 2) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys))))))

(defn piece-can-rotate? [piece-type board]
  (let [new-xs-ys (board->rotated-active-xys piece-type board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

;; TODO rewrite
(defn board-has-4-in-a-row? [board]
  (loop [rows (reverse board) col nil num 1]
    (let [r (first rows)
          pos (positions nil? r)
          count-pos (count pos)]
      (cond (not= count-pos 1) false
            (and (not (nil? col)) (not= (first pos) col)) false
            (and (= (first pos) col) (= num 4)) true
            :else (recur (rest rows) (first pos) (inc num))))))
