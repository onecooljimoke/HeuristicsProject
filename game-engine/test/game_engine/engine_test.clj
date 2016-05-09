(ns game-engine.engine-test
  (:use midje.sweet)
  (:require [game-engine.engine.engine :refer :all]))

(facts "Convert a column and row to an index in the field vector using a vector argument"
  (fact "'0' '0' returns 0"
    (col-row->field-index ["0" "0"]) => 0)
  (fact "'8' '8' returns 80"
    (col-row->field-index ["8" "8"]) => 80)
  (fact "'5' '4' returns 41"
    (col-row->field-index ["5" "4"]) => 41)
  (fact "'2' '7' returns 65"
    (col-row->field-index ["2" "7"]) => 65))

(facts "Convert a column and row to an index in the field vector using 2 int arguments"
  (fact "0 0 returns 0"
    (col-row->field-index 0 0) => 0)
  (fact "'8' '8' returns 80"
    (col-row->field-index 8 8) => 80)
  (fact "'5' '4' returns 41"
    (col-row->field-index 5 4) => 41)
  (fact "'2' '7' returns 65"
    (col-row->field-index 2 7) => 65))

(facts "Convert column and row to a macroboard"
  (fact "0 0 returns 0"
    (col-row->macroboard 0 0) => 0)
  (fact "4 2 returns 1"
    (col-row->macroboard 4 2) => 1)
  (fact "6 1 returns 2"
    (col-row->macroboard 6 1) => 2)
  (fact "2 3 returns 3"
    (col-row->macroboard 2 3) => 3)
  (fact "4 5 returns 4"
    (col-row->macroboard 4 5) => 4)
  (fact "7 4 returns 5"
    (col-row->macroboard 7 4) => 5)
  (fact "0 6 returns 6"
    (col-row->macroboard 0 6) => 6)
  (fact "4 7 returns 7"
    (col-row->macroboard 4 7) => 7)
  (fact "8 8 returns 8"
    (col-row->macroboard 8 8) => 8))

(facts "Validates macroboard placement"
  (let [macroboard ["-1", "0", "0", "0", "-1", "0", "-1", "0", "0"]]
    (fact "0 0 is valid"
      (macroboard-placement-valid? macroboard 0 0) => true)
    (fact "2 1 is valid"
      (macroboard-placement-valid? macroboard 2 1) => true)
    (fact "4 2 invalid"
      (macroboard-placement-valid? macroboard 4 2) => false)
    (fact "5 0 invalid"
      (macroboard-placement-valid? macroboard 5 0) => false)
    (fact "8 2 invalid"
      (macroboard-placement-valid? macroboard 8 2) => false)
    (fact "5 0 invalid"
      (macroboard-placement-valid? macroboard 5 0) => false)
    (fact "4 4 is valid"
      (macroboard-placement-valid? macroboard 4 4) => true)
    (fact "5 8 invalid"
      (macroboard-placement-valid? macroboard 5 8) => false)
    (fact "2 8 is valid"
      (macroboard-placement-valid? macroboard 2 8) => true)
    (fact "3 6 invalid"
      (macroboard-placement-valid? macroboard 3 6) => false)
    (fact "6 8 invalid"
      (macroboard-placement-valid? macroboard 6 8) => false)))

(facts "validate a move"
  (let [macroboard-vector ["-1", "-1", "-1", "0", "0", "0" "0", "0", "0"]
        field-vector1 (vec (repeat 81 "1"))
        field-vector2 (assoc field-vector1 0 "0" 6 "0")]
    (fact "valid move"
      (validate-requested-move "place_move 0 0" macroboard-vector field-vector2) => true)
    (fact "valid move"
      (validate-requested-move "place_move 6 0" macroboard-vector field-vector2) => true)
    (fact "invalid move"
      (validate-requested-move "place_move 4 5" macroboard-vector field-vector1) => false)
    (fact "invalid move"
      (validate-requested-move "place_move 0 0" macroboard-vector field-vector1) => false)))

(facts "Determine from a move where the next move must be"
  (fact "0 0 returns 0"
    (macroboard-index-from-move 0 0) => 0)
  (fact "2 1 returns 5"
    (macroboard-index-from-move 2 1) => 5)
  (fact "6 4 returns 3"
    (macroboard-index-from-move 6 4) => 3)
  (fact "4 5 returns 7"
    (macroboard-index-from-move 4 5) => 7))

(facts "transform macroboard vector for output"
  (let [macro-vector ["-1" "0" "1" "2" "-1" "0" "1" "2" "-1"]
        state-map1 {:macroboard-vector macro-vector 
                   :macroboard-move-index 0
                   :move-col 1
                   :move-row 1}
        state-map2 {:macroboard-vector macro-vector 
                    :macroboard-move-index 0
                    :move-col 1
                    :move-row 0}]
    (fact "required macroboard move is open in the board"
      (transform-macroboard-output state-map1) => ["0" "0" "1" "2" "-1" "0" "1" "2" "0"])
    (fact "required macroboard move is not open in the board"
      (transform-macroboard-output state-map2) => macro-vector)))

(facts "update field vector"
  (fact "test 1"
      (let [state-map {:field-vector (vec (repeat 81 "0"))
                   :move-col 0
                   :move-row 0
                   :moving-player :bot1}]
        ((update-field-vector state-map) 0) => "1"))
  (fact "test 2"
      (let [state-map {:field-vector (vec (repeat 81 "0"))
                   :move-col 4
                   :move-row 7
                   :moving-player :bot2}]
        ((update-field-vector state-map) 67) => "2")))

(facts "build rows, columns and diagonals"
  (let [board-vector ["1" "2" "3" "4" "5" "6" "7" "8" "9"]]
      (fact "build row"
        (build-row-list board-vector 1) => '("4" "5" "6"))
      (fact "build column"
        (build-column-list board-vector 2) => '("3" "6" "9"))
      (fact "build left diagonal"
        (build-left-diagonal board-vector) => '("1" "5" "9"))
      (fact "build right diagonal"
        (build-right-diagonal board-vector) => '("3" "5" "7"))))

(facts "check if a player won a board"
  (let [winning-row ["1" "1" "1" "2" "0" "2" "0" "0" "0"]
        winning-col ["1" "2" "2" "1" "0" "0" "1" "2" "0"]
        winning-diag ["1" "0" "0" "0" "1" "0" "0" "0" "1"]]
    (fact "winning row returns true for player 1"
      (board-won? (build-check-list winning-row 0 0) "1") => true)
    (fact "winning column returns true for player 1"
      (board-won? (build-check-list winning-col 0 0) "1") => true)
    (fact "winning diag returns true for player 1"
      (board-won? (build-check-list winning-diag 1 1) "1") => true)
    (fact "winning row returns false for player 2"
      (board-won? winning-row "2") => false)))
