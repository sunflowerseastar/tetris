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

(defn piece-matrix->xys
  "Given a 'piece matrix' and an active piece 'top-left' [x y], return all the corresponding xy coordinates on the board.
  Ex. [[1 1] [1 1]] [0 0] -> [[0 0] [1 0] [0 1] [1 1]]"
  ([piece-matrix [active-piece-top-left-x active-piece-top-left-y]]
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
  (let [piece-matrix (nth (get-in active-piece-game-state [:active-piece :xs-ys-rotations]) (:active-rotation-index active-piece-game-state))]
    (piece-matrix->xys piece-matrix [(:active-piece-top-left-x active-piece-game-state) (:active-piece-top-left-y active-piece-game-state)])))

(defn cycle-rotation-index
  "Given the current rotation index and the xs-ys-rotations, return the next
  rotation index."
  [active-rotation-index xs-ys-rotations]
  (mod (inc active-rotation-index) (count xs-ys-rotations)))

(defn rotate
  "Given the game state that contains all the active piece information, determine
  what the active piece's next rotation index would be, then translate the
  active piece's corresponding piece matrix to xys."
  [active-piece-game-state]
  (let [{:keys [active-rotation-index active-piece active-piece-top-left-x active-piece-top-left-y]} active-piece-game-state
        rotations (:xs-ys-rotations active-piece)
        new-rotation-index (cycle-rotation-index active-rotation-index rotations)
        rotated-piece-matrix (nth (get-in active-piece-game-state [:active-piece :xs-ys-rotations]) new-rotation-index)]
    (piece-matrix->xys rotated-piece-matrix [active-piece-top-left-x active-piece-top-left-y])))

(defn piece-can-rotate? [active-piece-game-state]
  (are-shifted-active-xys-in-bounds-and-free? (rotate active-piece-game-state) (:board active-piece-game-state)))

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
