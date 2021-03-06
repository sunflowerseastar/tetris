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
  (vec (map (fn [row] (vec (map #(if (true? (:active %)) nil %) row))) board)))

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
        min-y (reduce min ys)
        max-y (reduce max ys)
        y-in-bounds (and (>= min-y 0) (< max-y board-height))]
    (and x-in-bounds y-in-bounds)))

(defn xs-ys-are-free? [xs-ys board]
  (->> xs-ys
       (map #(get-x-y board %))
       (filter #(not (true? (:active %))))
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

(defn piece-can-move? [shift-fn board]
  (let [new-xs-ys (shift-fn board)
        in-bounds (xs-ys-in-bounds? new-xs-ys board)]
    (and in-bounds (xs-ys-are-free? new-xs-ys board))))

(defn piece-can-move-down? [board]
  (piece-can-move? board->shifted-down-active-xs-ys board))

(defn piece-can-move-left? [board]
  (piece-can-move? board->shifted-left-active-xs-ys board))

(defn piece-can-move-right? [board]
  (piece-can-move? board->shifted-right-active-xs-ys board))

(defn board->rotated-active-xs-ys [piece-type board]
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
  (let [new-xs-ys (board->rotated-active-xs-ys piece-type board)
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

(defn board-has-4-in-a-row? [board]
  (loop [rows (reverse board) col nil num 1]
    (let [r (first rows)
          pos (positions nil? r)
          count-pos (count pos)]
      (cond (not= count-pos 1) false
            (and (not (nil? col)) (not= (first pos) col)) false
            (and (= (first pos) col) (= num 4)) true
            :else (recur (rest rows) (first pos) (inc num))))))
