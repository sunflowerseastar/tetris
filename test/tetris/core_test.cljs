(ns tetris.core-test
  (:require
   [tetris.helpers :as helpers]
   [cljs.test :refer-macros [deftest is testing]]))

;; █ is active
(def sample-board-1
  "----
   ----
   -███
   -█--
   ----"
  [[nil nil nil nil]
   [nil nil nil nil]
   [nil nil nil nil]
   [nil
    {:color "#91cdf2", :active true, :x 1, :y 3}
    {:color "#91cdf2", :active true, :x 2, :y 3}
    {:color "#91cdf2", :active true, :x 3, :y 3}]
   [nil {:color "#91cdf2", :active true, :x 1, :y 4} nil nil]
   [nil nil nil nil]])

;; █ is active, □ is inactive
(def sample-board-2
  "----
   -█--
   -██
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

(deftest generate-blank-row
  (is (= (helpers/generate-blank-row 2) [nil nil])))

(deftest generate-board-test
  (is (= (helpers/generate-board 2 3)
         [[nil nil] [nil nil] [nil nil]])))

(deftest get-square-test
  (is (= (helpers/get-square sample-board-1 [1 3]) {:color "#91cdf2", :active true, :x 1, :y 3})))

(deftest get-actives-test
  (is (= (helpers/get-actives sample-board-1)
         '({:color "#91cdf2", :active true, :x 1, :y 3}
           {:color "#91cdf2", :active true, :x 2, :y 3}
           {:color "#91cdf2", :active true, :x 3, :y 3}
           {:color "#91cdf2", :active true, :x 1, :y 4}))))

(deftest board->shifted-down-active-xys-test
  (is (= (helpers/board->shifted-down-active-xys sample-board-1)
         '([1 4] [2 4] [3 4] [1 5]))))

(deftest board->shifted-left-active-xys-test
  (is (= (helpers/board->shifted-left-active-xys sample-board-1)
         '([0 3] [1 3] [2 3] [0 4]))))

(deftest board->shifted-right-active-xys-test
  (is (= (helpers/board->shifted-right-active-xys sample-board-1)
         '([2 3] [3 3] [4 3] [2 4]))))

(deftest xs-ys-in-bounds-test
  (is (= (helpers/xs-ys-in-bounds? (helpers/board->shifted-down-active-xys sample-board-1) sample-board-1) true))
  (is (= (helpers/xs-ys-in-bounds? (helpers/board->shifted-left-active-xys sample-board-1) sample-board-1) true))
  (is (= (helpers/xs-ys-in-bounds? (helpers/board->shifted-right-active-xys sample-board-1) sample-board-1) false)))

(deftest xs-ys-are-free-test
  (is (= (helpers/xs-ys-are-free? (helpers/board->shifted-down-active-xys sample-board-2) sample-board-2) false))
  (is (= (helpers/xs-ys-are-free? (helpers/board->shifted-left-active-xys sample-board-2) sample-board-2) true))
  (is (= (helpers/xs-ys-are-free? (helpers/board->shifted-right-active-xys sample-board-2) sample-board-2) false)))

(deftest piece-can-move-down-test
  (is (= (helpers/piece-can-move-down? sample-board-1) true))
  (is (= (helpers/piece-can-move-down? sample-board-2) false)))

(deftest piece-can-move-left-test
  (is (= (helpers/piece-can-move-left? sample-board-1) true))
  (is (= (helpers/piece-can-move-left? sample-board-2) true)))

(deftest piece-can-move-right-test
  (is (= (helpers/piece-can-move-right? sample-board-1) false))
  (is (= (helpers/piece-can-move-right? sample-board-2) false)))
