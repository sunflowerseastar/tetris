(ns tetris.helpers)

(defn get-square [x y board]
  (-> board (nth y) (nth x)))
(defn piece-can-move-down-p [board board-height]
  (let [actives (->> board
                     flatten
                     (filter #(= (:active %) true)))
        max-y (->> actives (map :y) (reduce max))
        has-reached-bottom-p (>= (inc max-y) board-height)]
    (if has-reached-bottom-p false
        (let [some-squares-are-non-empty-p (->> actives
                                                (map (fn [square] (get-square (:x square) (inc (:y square)) board)))
                                                (filter #(not (:active %)))
                                                (map empty?)
                                                (some false?))]
          (not some-squares-are-non-empty-p)))))
