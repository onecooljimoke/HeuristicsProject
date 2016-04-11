(ns basicbot.io-test
  (:use midje.sweet)
  (:require [basicbot.io.io :refer :all]))

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
