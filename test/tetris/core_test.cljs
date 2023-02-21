(ns tetris.core-test
  (:require
   [tetris.helpers :as helpers]
   [cljs.test :refer-macros [deftest is testing]]))

;; █ is active
(def test-game-1
  "----
   -███
   -█--
   ----"
  {:active-piece-top-left-x 1
   :active-piece-top-left-y 0
   :active-rotation-index 0
   :active-piece {:piece-type :l2
                  :starting-y-offset -1
                  :piece-matrix-rotations [[[0 0 0] [1 1 1] [1 0 0]]
                                           [[1 1 0] [0 1 0] [0 1 0]]
                                           [[0 0 1] [1 1 1] [0 0 0]]
                                           [[0 1 0] [0 1 0] [0 1 1]]]}
   :board [[nil nil nil nil]
           [nil
            {:color "#91cdf2", :active true}
            {:color "#91cdf2", :active true}
            {:color "#91cdf2", :active true}]
           [nil {:color "#91cdf2", :active true} nil nil]
           [nil nil nil nil]]})
;; "t1" is short for test-1, t2 for test-2, etc.
(def t1-board (:board test-game-1))
(def t1-active-piece-coords (helpers/active-piece-game-state->active-piece-coords test-game-1))

;; █ is active, □ is inactive
(def test-game-2
  "----
   -█--
   -██-
   -█□□
   -□□-"
  {:active-piece-top-left-x 0
   :active-piece-top-left-y 1
   ;; :active-piece-type nil
   ;; :active-piece-color nil
   :active-rotation-index 3
   :active-piece {:piece-type :t
                  :starting-y-offset -1
                  ;; :color-rgb-hex (:yellow colors)
                  :rotation 0
                  :piece-matrix-rotations [[[0 0 0] [1 1 1] [0 1 0]]
                                           [[0 1 0] [1 1 0] [0 1 0]]
                                           [[0 1 0] [1 1 1] [0 0 0]]
                                           [[0 1 0] [0 1 1] [0 1 0]]]}
   :board [[nil nil nil nil]
           [nil {:color "#faedb9", :active true} nil nil]
           [nil
            {:color "#faedb9", :active true}
            {:color "#faedb9", :active true}
            nil]
           [nil
            {:color "#faedb9", :active true}
            {:color "#b1e597", :active false}
            {:color "#b1e597", :active false}]
           [nil
            {:color "#b1e597", :active false}
            {:color "#b1e597", :active false}
            nil]]})
(def t2-board (:board test-game-2))
(def t2-active-piece-coords (helpers/active-piece-game-state->active-piece-coords test-game-2))

;; █ is active, □ is inactive
(def test-game-3
  "----
   -███
   -█□□
   -□□-"
  {:active-piece-top-left-x 1
   :active-piece-top-left-y 0
   :active-rotation-index 0
   :active-piece {:piece-type :l2
                  :piece-matrix-rotations [[[0 0 0] [1 1 1] [0 1 0]]
                                           [[0 1 0] [1 1 0] [0 1 0]]
                                           [[0 1 0] [1 1 1] [0 0 0]]
                                           [[0 1 0] [0 1 1] [0 1 0]]]}
   :board [[nil nil nil nil]
           [nil
            {:color "#91cdf2", :active true}
            {:color "#91cdf2", :active true}
            {:color "#91cdf2", :active true}]
           [nil {:color "#91cdf2", :active true}
            {:color "#b1e597", :active false}
            {:color "#b1e597", :active false}]
           [nil
            {:color "#b1e597", :active false}
            {:color "#b1e597", :active false}
            nil]]})
(def t3-active-piece-coords (helpers/active-piece-game-state->active-piece-coords test-game-3))

(deftest generate-blank-row
  (is (= (helpers/generate-blank-row 2) [nil nil])))

(deftest generate-board-test
  (is (= (helpers/generate-board 2 3)
         [[nil nil] [nil nil] [nil nil]])))

(deftest get-square-test
  (is (= (helpers/get-square (:board test-game-1) [1 2]) {:color "#91cdf2", :active true})))

(deftest active-piece-game-state->active-piece-coords-test
  (is (= (helpers/active-piece-game-state->active-piece-coords test-game-1) [[1 1] [2 1] [3 1] [1 2]]))
  (is (= (helpers/active-piece-game-state->active-piece-coords test-game-2) [[1 1] [1 2] [2 2] [1 3]]))
  (is (= (helpers/active-piece-game-state->active-piece-coords test-game-3) [[1 1] [2 1] [3 1] [2 2]])))

(deftest are-shifted-active-coords-in-bounds-and-free-test
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? t1-active-piece-coords t1-board) true))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-left t1-active-piece-coords) t1-board) true))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-right t1-active-piece-coords) t1-board) false))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-down t1-active-piece-coords) t1-board) true))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? t2-active-piece-coords t2-board) true))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-left t2-active-piece-coords) t2-board) true))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-right t2-active-piece-coords) t2-board) false))
  (is (= (helpers/are-shifted-active-coords-in-bounds-and-free? (helpers/shift-piece-matrix-down t2-active-piece-coords) t2-board) false)))

(deftest coords-in-bounds-test
  (is (= (helpers/coords-in-bounds? t1-active-piece-coords t1-board) true))
  (is (= (helpers/coords-in-bounds? (helpers/shift-piece-matrix-down t1-active-piece-coords) t1-board) true))
  (is (= (helpers/coords-in-bounds? (helpers/shift-piece-matrix-left t1-active-piece-coords) t1-board) true))
  (is (= (helpers/coords-in-bounds? (helpers/shift-piece-matrix-right t1-active-piece-coords) t1-board) false)))

(deftest coords-are-free-test
  (is (= (helpers/coords-are-free? t1-active-piece-coords t1-board) true))
  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-down t1-active-piece-coords) t1-board) true))
  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-left t1-active-piece-coords) t1-board) true))
  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-right t1-active-piece-coords) t1-board) true))

  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-down t2-active-piece-coords) t2-board) false))
  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-left t2-active-piece-coords) t2-board) true))
  (is (= (helpers/coords-are-free? (helpers/shift-piece-matrix-right t2-active-piece-coords) t2-board) false)))

(deftest shift-piece-matrix-down-test
  (is (= (helpers/shift-piece-matrix-down t1-active-piece-coords) '([1 2] [2 2] [3 2] [1 3])))
  (is (= (helpers/shift-piece-matrix-down t2-active-piece-coords) '([1 2] [1 3] [2 3] [1 4]))))

(deftest shift-piece-matrix-left-test
  (is (= (helpers/shift-piece-matrix-left t1-active-piece-coords) '([0 1] [1 1] [2 1] [0 2])))
  (is (= (helpers/shift-piece-matrix-left t2-active-piece-coords) '([0 1] [0 2] [1 2] [0 3]))))

(deftest shift-piece-matrix-right-test
  (is (= (helpers/shift-piece-matrix-right t1-active-piece-coords) '([2 1] [3 1] [4 1] [2 2])))
  (is (= (helpers/shift-piece-matrix-right t2-active-piece-coords) '([2 1] [2 2] [3 2] [2 3]))))

(deftest piece-can-move-down-test
  (is (= (helpers/piece-can-move-down? t1-active-piece-coords (:board test-game-1)) true))
  (is (= (helpers/piece-can-move-down? t2-active-piece-coords (:board test-game-2)) false)))

(deftest piece-can-move-left-test
  ;; can move it left once
  (is (= (helpers/piece-can-move-left? t1-active-piece-coords (:board test-game-1)) true))
  ;; can't move it left twice
  (is (= (helpers/piece-can-move-left? (helpers/shift-piece-matrix-left t1-active-piece-coords) (:board test-game-1)) false))
  (is (= (helpers/piece-can-move-left? t2-active-piece-coords (:board test-game-2)) true)))

(deftest piece-can-move-right-test
  (is (= (helpers/piece-can-move-right? t1-active-piece-coords (:board test-game-1)) false))
  (is (= (helpers/piece-can-move-right? t2-active-piece-coords (:board test-game-2)) false)))

(deftest cycle-rotation-index-test
  (is (= (helpers/cycle-rotation-index (:active-rotation-index test-game-1) (get-in test-game-1 [:active-piece :piece-matrix-rotations])) 1))
  (is (= (helpers/cycle-rotation-index (:active-rotation-index test-game-2) (get-in test-game-2 [:active-piece :piece-matrix-rotations])) 0))
  (is (= (helpers/cycle-rotation-index (:active-rotation-index test-game-3) (get-in test-game-3 [:active-piece :piece-matrix-rotations])) 1)))

(deftest rotate-test
  (is (= (helpers/rotate test-game-1) [[1 0] [2 0] [2 1] [2 2]]))
  (is (= (helpers/rotate test-game-2) [[0 2] [1 2] [2 2] [1 3]]))
  (is (= (helpers/rotate test-game-3) [[2 0] [1 1] [2 1] [2 2]])))

(deftest piece-can-rotate-test
  (is (= (helpers/piece-can-rotate? test-game-1) true))
  (is (= (helpers/piece-can-rotate? test-game-2) true))
  (is (= (helpers/piece-can-rotate? test-game-3) false)))
