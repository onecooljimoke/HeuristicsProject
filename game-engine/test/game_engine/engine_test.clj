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

(facts "transform macroboard vector for output"
  (let [macro-vec ["-1" "0" "1" "2" "-1" "0" "1" "2" "-1"]]
    (fact "required macroboard move is open in the board"
      (transform-macroboard-output macro-vec 4) => ["0" "0" "1" "2" "-1" "0" "1" "2" "0"])
    (fact "required macroboard move is not open in the board"
      (transform-macroboard-output macro-vec 7) => macro-vec)))
