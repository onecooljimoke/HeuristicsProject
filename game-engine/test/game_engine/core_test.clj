(ns game-engine.core-test
  (:use midje.sweet)
  (:require [game-engine.core :refer :all]))

(facts "Some example tests"
  (fact "this is true"
    (= 1 1) => true)
  (fact "this is false"
    (= 1 0) => false)
  (fact "this is also true"
    (let [x 1]
      (= x 1) => true)))
