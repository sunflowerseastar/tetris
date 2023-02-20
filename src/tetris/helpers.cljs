(ns tetris.helpers)

(defn generate-blank-row [width]
  (vec (repeat width nil)))

(defn generate-board [board-width board-height]
  (vec (repeat board-height (generate-blank-row board-width))))

(defn board->board-without-completions [board board-width]
  (let [blank-row (generate-blank-row board-width)]
    (loop [rows board new-board [] num-rows-completed 0]
      (if (empty? rows) [new-board num-rows-completed]
          (let [row (first rows)
                is-completed (->> row (map #(not (nil? %))) (every? true?))]
            (if is-completed
              ;; (recur (rest rows) (prepend blank-row new-board) (inc num-rows-completed))
              (recur (rest rows) (vec (cons blank-row new-board)) (inc num-rows-completed))
              (recur (rest rows) (conj new-board row) num-rows-completed)))))))

(defn board->board-without-actives [board]
  (mapv (fn [row] (mapv #(if (true? (:active %)) nil %) row)) board))

;; TODO refactor x-y and xys to coordinates?
(defn x-y-in-bounds? [[x y] board]
  (let [x-in-bounds (and (>= x 0) (< x (count (first board))))
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

(defn are-shifted-active-xys-in-bounds-and-free?
  "Given the xy's of a shifted active piece and a board, return a boolean whether
  it's a legal shift or not. The following three functions use this to see if
  the active piece can shift down, left, and right."
  [shifted-active-xys board]
  (and (xs-ys-in-bounds? shifted-active-xys board)
       (xs-ys-are-free? shifted-active-xys board)))

;; --------------------------
;; old
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

;; new
(defn shift-piece-matrix-down
  "Given the current active piece xy's, return the xy's shifted down one square."
  [capm]
  (map (fn [[x y]] [x (inc y)]) capm))

(defn shift-piece-matrix-left
  "Given the current active piece xy's, return the xy's shifted left one square."
  [capm]
  (map (fn [[x y]] [(dec x) y]) capm))

(defn shift-piece-matrix-right
  "Given the current active piece xy's, return the xy's shifted right one square."
  [capm]
  (map (fn [[x y]] [(inc x) y]) capm))

(defn piece-can-move-down? [active-xys board]
  (are-shifted-active-xys-in-bounds-and-free?
   (shift-piece-matrix-down active-xys) board))

(defn piece-can-move-left? [active-xys board]
  (are-shifted-active-xys-in-bounds-and-free?
   (shift-piece-matrix-left active-xys) board))

(defn piece-can-move-right? [active-xys board]
  (are-shifted-active-xys-in-bounds-and-free?
   (shift-piece-matrix-right active-xys) board))
;; --------------------------

(defn piece-matrix->xys
  "Given a 'piece matrix' and an active piece 'top-left' [x y], return all the corresponding xy coordinates on the board.
  Ex. [[1 1] [1 1]] [0 0] -> [[0 0] [1 0] [0 1] [1 1]]"
  ([piece-matrix [active-piece-top-left-x active-piece-top-left-y]]
   ;; (println "piece-matrix->xys:" piece-matrix active-piece-top-left-x active-piece-top-left-y)
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

(defn active-piece-game-state->active-piece-xys
  "Given the game state that describes the active piece, return the xys of the
  active piece."
  [active-piece-game-state]
  (let [piece-matrix (nth (get-in active-piece-game-state [:active-piece :xs-ys-rotations]) (:active-piece-rotation active-piece-game-state))]
    (piece-matrix->xys piece-matrix [(:active-piece-top-left-x active-piece-game-state) (:active-piece-top-left-y active-piece-game-state)])))

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
              (let [
                    ;; block-1 (it-> actives (first it) [(:x it) (:y it)])
                    block-1 (as-> actives it (first it) [(:x it) (:y it)])
                    block-2 (as-> actives it (nth it 3) [(:x it) (:y it)])
                    block-3 (as-> actives it (first it) [(dec (:x it)) (:y it)])
                    block-4 (as-> actives it (first it) [(dec (:x it)) (dec (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              (let [
                    block-1 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-2 (as-> actives it (nth it 2) [(inc (:x it)) (:y it)])
                    block-3 (as-> actives it (nth it 3) [(dec (:x it)) (:y it)])
                    block-4 (as-> actives it (nth it 3) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :s2)
          (let [max-x (reduce max xs)
                min-x (reduce min xs)
                delta-x (- max-x min-x)
                is-pos-1 (= delta-x 2)]
            (if is-pos-1
              (let [
                    block-1 (as-> actives it (second it) [(inc (:x it)) (dec (:y it))])
                    block-2 (as-> actives it (second it) [(:x it) (:y it)])
                    block-3 (as-> actives it (second it) [(inc (:x it)) (:y it)])
                    block-4 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              (let [
                    block-1 (as-> actives it (second it) [(dec (:x it)) (:y it)])
                    block-2 (as-> actives it (second it) [(:x it) (:y it)])
                    block-3 (as-> actives it (nth it 3) [(:x it) (:y it)])
                    block-4 (as-> actives it (nth it 3) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :l1)
          (let [max-x (reduce max xs)
                is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (= (count (filter #(= % max-x) xs)) 3)
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond
              is-pos-1
              (let [block-1 (as-> actives it (second it) [(:x it) (dec (:y it))])
                    block-2 (as-> actives it (second it) [(:x it) (:y it)])
                    block-3 (as-> actives it (second it) [(:x it) (inc (:y it))])
                    block-4 (as-> actives it (first it) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-2
              (let [block-1 (as-> actives it (first it) [(dec (:x it)) (:y it)])
                    block-2 (as-> actives it (second it) [(dec (:x it)) (:y it)])
                    block-3 (as-> actives it (second it) [(:x it) (:y it)])
                    block-4 (as-> actives it (second it) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-3
              (let [block-1 (as-> actives it (first it) [(inc (:x it)) (:y it)])
                    block-2 (as-> actives it (first it) [(+ 2 (:x it)) (:y it)])
                    block-3 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-4 (as-> actives it (nth it 2) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              :else
              (let [block-1 (as-> actives it (nth it 2) [(dec (:x it)) (:y it)])
                    block-2 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-3 (as-> actives it (nth it 2) [(inc (:x it)) (:y it)])
                    block-4 (as-> actives it (nth it 3) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)))
          (= piece-type :l2)
          (let [max-x (reduce max xs)
                is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (= (count (filter #(= % max-x) xs)) 3)
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond is-pos-1
                  (let [block-1 (as-> actives it (first it) [(:x it) (dec (:y it))])
                        block-2 (as-> actives it (second it) [(:x it) (dec (:y it))])
                        block-3 (as-> actives it (second it) [(:x it) (:y it)])
                        block-4 (as-> actives it (second it) [(:x it) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-2
                  (let [block-1 (as-> actives it (first it) [(:x it) (inc (:y it))])
                        block-2 (as-> actives it (nth it 2) [(:x it) (:y it)])
                        block-3 (as-> actives it (nth it 2) [(inc (:x it)) (:y it)])
                        block-4 (as-> actives it (second it) [(inc (:x it)) (:y it)])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  is-pos-3
                  (let [block-1 (as-> actives it (first it) [(dec (:x it)) (:y it)])
                        block-2 (as-> actives it (nth it 2) [(:x it) (:y it)])
                        block-3 (as-> actives it (nth it 2) [(:x it) (inc (:y it))])
                        block-4 (as-> actives it (nth it 3) [(:x it) (inc (:y it))])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)
                  :else
                  (let [block-1 (as-> actives it (second it) [(dec (:x it)) (:y it)])
                        block-2 (as-> actives it (second it) [(:x it) (:y it)])
                        block-3 (as-> actives it (second it) [(inc (:x it)) (:y it)])
                        block-4 (as-> actives it (nth it 2) [(dec (:x it)) (:y it)])
                        new-xs-ys [block-1 block-2 block-3 block-4]]
                    new-xs-ys)))
          (= piece-type :t)
          (let [is-pos-1 (apply = [(first ys) (second ys) (nth ys 2)])
                is-pos-2 (apply = [(first xs) (second xs) (nth xs 3)])
                is-pos-3 (apply = [(second ys) (nth ys 2) (nth ys 3)])]
            (cond
              is-pos-1
              (let [block-1 (as-> actives it (second it) [(:x it) (dec (:y it))])
                    block-2 (as-> actives it (first it) [(:x it) (:y it)])
                    block-3 (as-> actives it (second it) [(:x it) (:y it)])
                    block-4 (as-> actives it (nth it 3) [(:x it) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-2
              (let [block-1 (as-> actives it (second it) [(dec (:x it)) (:y it)])
                    block-2 (as-> actives it (second it) [(:x it) (:y it)])
                    block-3 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-4 (as-> actives it (second it) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              is-pos-3
              (let [
                    block-1 (as-> actives it (first it) [(:x it) (:y it)])
                    block-2 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-3 (as-> actives it (nth it 3) [(:x it) (:y it)])
                    block-4 (as-> actives it (nth it 2) [(:x it) (inc (:y it))])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys)
              :else
              (let [block-1 (as-> actives it (first it) [(:x it) (:y it)])
                    block-2 (as-> actives it (second it) [(:x it) (:y it)])
                    block-3 (as-> actives it (nth it 2) [(:x it) (:y it)])
                    block-4 (as-> actives it (nth it 2) [(inc (:x it)) (:y it)])
                    new-xs-ys [block-1 block-2 block-3 block-4]]
                new-xs-ys))))))

;; TODO make this get enough params to use the rotation number in state
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
