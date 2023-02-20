(ns tetris.core-test
  (:require
   [tetris.helpers :as helpers]
   [cljs.test :refer-macros [deftest is testing]]))

;; "sg" means "sample-game" throughout

;; █ is active
(def sample-board-1
  "----
   -███
   -█--
   ----"
  [[nil nil nil nil]
   [nil
    {:color "#91cdf2", :active true, :x 1, :y 1}
    {:color "#91cdf2", :active true, :x 2, :y 1}
    {:color "#91cdf2", :active true, :x 3, :y 1}]
   [nil {:color "#91cdf2", :active true, :x 1, :y 2} nil nil]
   [nil nil nil nil]])

;; █ is active
(def sample-game-1
  "----
   -███
   -█--
   ----"
  {:active-piece-top-left-x 1
   :active-piece-top-left-y 0
   :active-rotation-index 0
   :active-piece {:piece-type :l2
                  :starting-y-offset -1
                  :xs-ys-rotations [[[0 0 0] [1 1 1] [1 0 0]]
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
(def sg1-board (:board sample-game-1))
(def sg1-active-piece-xys (helpers/active-piece-game-state->active-piece-xys sample-game-1))

;; █ is active, □ is inactive
(def sample-board-2
  "----
   -█--
   -██-
   -█□□
   -□□-"
  [[nil nil nil nil]
   [nil nil nil nil]
   [nil {:color "#faedb9", :active true, :x 1, :y 2} nil nil]
   [nil
    {:color "#faedb9", :active true, :x 1, :y 3}
    {:color "#faedb9", :active true, :x 2, :y 3}
    nil]
   [nil
    {:color "#faedb9", :active true, :x 1, :y 4}
    {:color "#b1e597", :active false, :x 2, :y 4}
    {:color "#b1e597", :active false, :x 3, :y 4}]
   [nil
    {:color "#b1e597", :active false, :x 1, :y 5}
    {:color "#b1e597", :active false, :x 2, :y 5}
    nil]])

;; █ is active, □ is inactive
(def sample-game-2
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
                  :xs-ys-rotations [[[0 0 0] [1 1 1] [0 1 0]]
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
(def sg2-board (:board sample-game-2))
(def sg2-active-piece-xys (helpers/active-piece-game-state->active-piece-xys sample-game-2))

;; █ is active, □ is inactive
(def sample-game-3
  "----
   -███
   -█□□
   -□□-"
  {:active-piece-top-left-x 1
   :active-piece-top-left-y 0
   :active-rotation-index 0
   :active-piece {:piece-type :l2
                  :xs-ys-rotations [[[0 0 0] [1 1 1] [0 1 0]]
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
(def sg3-active-piece-xys (helpers/active-piece-game-state->active-piece-xys sample-game-3))

(deftest generate-blank-row
  (is (= (helpers/generate-blank-row 2) [nil nil])))

(deftest generate-board-test
  (is (= (helpers/generate-board 2 3)
         [[nil nil] [nil nil] [nil nil]])))

(deftest get-square-test
  (is (= (helpers/get-square (:board sample-game-1) [1 2]) {:color "#91cdf2", :active true})))

(deftest active-piece-game-state->active-piece-xys-test
  (is (= (helpers/active-piece-game-state->active-piece-xys sample-game-1) [[1 1] [2 1] [3 1] [1 2]]))
  (is (= (helpers/active-piece-game-state->active-piece-xys sample-game-2) [[1 1] [1 2] [2 2] [1 3]]))
  (is (= (helpers/active-piece-game-state->active-piece-xys sample-game-3) [[1 1] [2 1] [3 1] [2 2]])))

(deftest are-shifted-active-xys-in-bounds-and-free-test
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? sg1-active-piece-xys sg1-board) true))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-left sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-right sg1-active-piece-xys) sg1-board) false))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-down sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? sg2-active-piece-xys sg2-board) true))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-left sg2-active-piece-xys) sg2-board) true))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-right sg2-active-piece-xys) sg2-board) false))
  (is (= (helpers/are-shifted-active-xys-in-bounds-and-free? (helpers/shift-piece-matrix-down sg2-active-piece-xys) sg2-board) false)))

(deftest xs-ys-in-bounds-test
  (is (= (helpers/xs-ys-in-bounds? sg1-active-piece-xys sg1-board) true))
  (is (= (helpers/xs-ys-in-bounds? (helpers/shift-piece-matrix-down sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/xs-ys-in-bounds? (helpers/shift-piece-matrix-left sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/xs-ys-in-bounds? (helpers/shift-piece-matrix-right sg1-active-piece-xys) sg1-board) false)))

(deftest xs-ys-are-free-test
  (is (= (helpers/xs-ys-are-free? sg1-active-piece-xys sg1-board) true))
  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-down sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-left sg1-active-piece-xys) sg1-board) true))
  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-right sg1-active-piece-xys) sg1-board) true))

  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-down sg2-active-piece-xys) sg2-board) false))
  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-left sg2-active-piece-xys) sg2-board) true))
  (is (= (helpers/xs-ys-are-free? (helpers/shift-piece-matrix-right sg2-active-piece-xys) sg2-board) false)))

(deftest shift-piece-matrix-down-test
  (is (= (helpers/shift-piece-matrix-down sg1-active-piece-xys) '([1 2] [2 2] [3 2] [1 3])))
  (is (= (helpers/shift-piece-matrix-down sg2-active-piece-xys) '([1 2] [1 3] [2 3] [1 4]))))

(deftest shift-piece-matrix-left-test
  (is (= (helpers/shift-piece-matrix-left sg1-active-piece-xys) '([0 1] [1 1] [2 1] [0 2])))
  (is (= (helpers/shift-piece-matrix-left sg2-active-piece-xys) '([0 1] [0 2] [1 2] [0 3]))))

(deftest shift-piece-matrix-right-test
  (is (= (helpers/shift-piece-matrix-right sg1-active-piece-xys) '([2 1] [3 1] [4 1] [2 2])))
  (is (= (helpers/shift-piece-matrix-right sg2-active-piece-xys) '([2 1] [2 2] [3 2] [2 3]))))

(deftest piece-can-move-down-test
  (is (= (helpers/piece-can-move-down? sg1-active-piece-xys (:board sample-game-1)) true))
  (is (= (helpers/piece-can-move-down? sg2-active-piece-xys (:board sample-game-2)) false)))

(deftest piece-can-move-left-test
  ;; can move it left once
  (is (= (helpers/piece-can-move-left? sg1-active-piece-xys (:board sample-game-1)) true))
  ;; can't move it left twice
  (is (= (helpers/piece-can-move-left? (helpers/shift-piece-matrix-left sg1-active-piece-xys) (:board sample-game-1)) false))
  (is (= (helpers/piece-can-move-left? sg2-active-piece-xys (:board sample-game-2)) true)))

(deftest piece-can-move-right-test
  (is (= (helpers/piece-can-move-right? sg1-active-piece-xys (:board sample-game-1)) false))
  (is (= (helpers/piece-can-move-right? sg2-active-piece-xys (:board sample-game-2)) false)))

(deftest cycle-rotation-index-test
  (is (= (helpers/cycle-rotation-index (:active-rotation-index sample-game-1) (get-in sample-game-1 [:active-piece :xs-ys-rotations])) 1))
  (is (= (helpers/cycle-rotation-index (:active-rotation-index sample-game-2) (get-in sample-game-2 [:active-piece :xs-ys-rotations])) 0))
  (is (= (helpers/cycle-rotation-index (:active-rotation-index sample-game-3) (get-in sample-game-3 [:active-piece :xs-ys-rotations])) 1)))

(deftest piece-can-rotate-test
  (is (= (helpers/piece-can-rotate? sample-game-1) true))
  (is (= (helpers/piece-can-rotate? sample-game-2) true))
  (is (= (helpers/piece-can-rotate? sample-game-3) false)))
