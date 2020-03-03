(ns tetris.helpers)

(defn get-square [x y board]
  (-> board (nth y) (nth x)))

(defn random-up-to [n]
  (js/parseInt (* (.random js/Math) n)))

(defn some-squares-are-non-empty-p [squares board]
  (->> squares
       (map (fn [square] (get-square (:x square) (inc (:y square)) board)))
       (filter #(not (= (:active %) true)))
       (map empty?)
       (some false?)))

(defn piece-can-move-down-p [board board-height]
  (let [actives (->> board
                     flatten
                     (filter #(= (:active %) true)))
        max-y (->> actives (map :y) (reduce max))
        has-reached-bottom-p (>= (inc max-y) board-height)]
    (println actives)
    (and (not has-reached-bottom-p) (not (some-squares-are-non-empty-p actives board)))))
