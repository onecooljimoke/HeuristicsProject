(ns basicbot.core-test
  (:use midje.sweet)
  (:require [basicbot.core :refer :all]))

(facts "test facts"
  (fact "this is true"
    (= 1 1) => true)
  (fact "this if false"
    (= 0 1) => false)
  (fact "this is also true"
    (let [x 1]
      (= 1 x) => true)))
