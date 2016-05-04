(ns game-engine.engine-test
  (:use midje.sweet)
  (:require [game-engine.engine.engine :refer :all]))

(facts "Convert a column and row to an index in the field vector"
  (fact "'0' '0' returns 0"
    (= 0  (row-col->field-index ["0" "0"])) => true)
  (fact "'8' '8' returns 80"
    (= 80 (row-col->field-index ["8" "8"])) => true)
  (fact "'5' '4' returns 41"
    (= 41 (row-col->field-index ["5" "4"])) => true)
  (fact "'2' '7' returns 65"
    (= 65 (row-col->field-index ["2" "7"])) => true))
