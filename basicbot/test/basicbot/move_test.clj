(ns basicbot.move-test
  (:use midje.sweet)
  (:require [basicbot.move.move :refer :all]))

; example tests
; basic form:
; (fact "description string" <function-to-check> => <expected-value) 
; we can wrap several fact statements in facts for logical grouping
; but it's not required
(facts "Some example tests"
  (fact "this is true"
    (= 1 1) => true)
  (fact "this if false"
    (= 0 1) => false)
  (fact "this is also true"
    (let [x 1]
      (= 1 x) => true)))

(facts "macroboard stage"
  (let [board-vec ["-1" "-1" "-1" "-1" "-1" "-1" "-1" "-1" "-1"]]
    (fact "returns an integer"
      (macroboard-stage board-vec) => integer?)))

(facts "output stage"
  (fact "output is a move string"
    (output-stage 0 0) => "place_move 0 0"))
