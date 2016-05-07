(ns game-engine.engine-test
  (:use midje.sweet)
  (:require [game-engine.engine.engine :refer :all]))

(facts "Convert a column and row to an index in the field vector"
  (fact "'0' '0' returns 0"
    (col-row->field-index ["0" "0"]) => 0)
  (fact "'8' '8' returns 80"
    (col-row->field-index ["8" "8"]) => 80)
  (fact "'5' '4' returns 41"
    (col-row->field-index ["5" "4"]) => 41)
  (fact "'2' '7' returns 65"
    (col-row->field-index ["2" "7"]) => 65))

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
      (validate-macroboard-placement 0 0 macroboard) => true)
    (fact "2 1 is valid"
      (validate-macroboard-placement 2 1 macroboard) => true)
    (fact "4 2 invalid"
      (validate-macroboard-placement 4 2 macroboard) => false)
    (fact "5 0 invalid"
      (validate-macroboard-placement 5 0 macroboard) => false)
    (fact "8 2 invalid"
      (validate-macroboard-placement 8 2 macroboard) => false)
    (fact "5 0 invalid"
      (validate-macroboard-placement 5 0 macroboard) => false)
    (fact "4 4 is valid"
      (validate-macroboard-placement 4 4 macroboard) => true)
    (fact "5 8 invalid"
      (validate-macroboard-placement 5 8 macroboard) => false)
    (fact "2 8 is valid"
      (validate-macroboard-placement 2 8 macroboard) => true)
    (fact "3 6 invalid"
      (validate-macroboard-placement 3 6 macroboard) => false)
    (fact "6 8 invalid"
      (validate-macroboard-placement 6 8 macroboard) => false)))
