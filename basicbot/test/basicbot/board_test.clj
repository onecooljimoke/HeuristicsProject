(ns basicbot.board-test
  (:use midje.sweet)
  (:require [basicbot.board.board :refer :all]
            [basicbot.board.board-math :refer :all]))

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

; testing a let inside a fact
(facts "string-to-vector"
  (fact "returns a vector of strings if rgx is a space"
    (let [input "this is a test"
          rgx #" "
          result ["this" "is" "a" "test"]]
      (string->vector input rgx) => result))
  (fact "returns a vector of strings if rgx is a comma"
    (let [input "this,is,a,test"
          rgx #","
          result ["this" "is" "a" "test"]]
      (string->vector input rgx) => result)))

; testing a let that wraps multiple facts
(facts "string-to-vector-2"
  (let [space-input "this is a test"
        comma-input "this,is,a,test"
        result ["this" "is" "a" "test"]]
    (fact "returns a vector of strings if rgx is a space"
      (string->vector space-input #" ") => result)
    (fact "returns a vector of strings if rgx is a comma"
      (string->vector comma-input #",") => result)))

(facts "determine opponent bot id correctly"
  (fact "if our bot id is '1', opponent bot id is '2'"
    (determine-opponent-id "1") => "2")
  (fact "if our bot id is '2', opponent bot id is '1'"
    (determine-opponent-id "2") => "1"))

; testing that bot ids get updated correctly
; since we're testing mutable state, we need to use with-state-changes
(with-state-changes [(before :facts (do
                                      (reset! our-bot-id "0")
                                      (reset! opponent-bot-id "0")))]
  (fact "if our bot id is 1, opponent bot id is 2"
    (game-input-starts-with-settings ["settings" "your_botid" "1"])
    (and
     (= @our-bot-id "1")
     (= @opponent-bot-id "2")) => true)
  (fact "if our bot id is 2, opponent bot id is 1"
    (game-input-starts-with-settings ["settings" "your_botid" "2"])
    (and
     (= @our-bot-id "2")
     (= @opponent-bot-id "1")) => true))

; testing that macroboard and field vectors get updated correctly
; testing atoms so use with-state-changes
(with-state-changes [(before :facts (do
                                      (reset! field-vector [])
                                      (reset! macroboard-vector [])))]
  (fact "the input is 'update game field"
    (game-input-starts-with-update ["update" "game" "field" "0,1,2"])
    (= @field-vector ["0" "1" "2"]) => true)
  (fact "the input is 'update game macroboard"
    (game-input-starts-with-update ["update" "game" "macroboard" "-1,0,1"])
    (= @macroboard-vector ["-1" "0" "1"])))

(facts "finding available macroboard squares"
  (fact "return a list of -1 or false values"
    (let [board-vec ["0" "1" "2" "-1" "0" "1" "2" "-1" "0"]
          result '(false false false 3 false false false 7 false)]
      (macroboard-move-list board-vec) => result))

  (fact "returns macroboard indices that don't have a value of -1"
    (let [board-vec ["0" "1" "2" "-1" "0" "1" "2" "-1" "0"]
          result '(3 7)]
      (big-squares-available board-vec) => result))

  (fact "returns empty list if no values of -1"
    (let [board-str "0,1,2,0,1,2,0,1,2"
          result '()]
      (big-squares-available board-str) => result)))

(facts "upper left macro column"
  (fact "is 0 for macroboard position 0, 3 and 6"
    (upper-left-macro-column 0) => 0
    (upper-left-macro-column 3) => 0
    (upper-left-macro-column 6) => 0)

  (fact "is 3 for macroboard position 1, 4 and 7"
    (upper-left-macro-column 1) => 3
    (upper-left-macro-column 4) => 3
    (upper-left-macro-column 7) => 3)

  (fact "is 6 for macroboard position 2, 5 and 8"
    (upper-left-macro-column 2) => 6
    (upper-left-macro-column 5) => 6
    (upper-left-macro-column 8) => 6))

(facts "upper left macro row"
  (fact "is 0 for macroboard position 0, 1 and 2"
    (upper-left-macro-row 0) => 0
    (upper-left-macro-row 1) => 0
    (upper-left-macro-row 2) => 0)

  (fact "is 3 for macroboard position 3, 4 and 5"
    (upper-left-macro-row 3) => 3
    (upper-left-macro-row 4) => 3
    (upper-left-macro-row 5) => 3)

  (fact "is 6 for macroboard position 6, 7 and 8"
    (upper-left-macro-row 6) => 6
    (upper-left-macro-row 7) => 6
    (upper-left-macro-row 8) => 6))

(facts "internal macroboard column"
  (fact "is 0 for macroboard position 0, 3 and 6"
    (internal-macroboard-column 0) => 0
    (internal-macroboard-column 3) => 0
    (internal-macroboard-column 6) => 0)

  (fact "is 1 for macroboard position 1, 4 and 7"
    (internal-macroboard-column 1) => 1
    (internal-macroboard-column 4) => 1
    (internal-macroboard-column 7) => 1)

  (fact "is 2 for macroboard position 2, 5 and 8"
    (internal-macroboard-column 2) => 2
    (internal-macroboard-column 5) => 2
    (internal-macroboard-column 8) => 2))

(facts "internal macroboard row"
  (fact "is 0 for index 0, 1 and 2"
    (internal-macroboard-row 0) => 0
    (internal-macroboard-row 1) => 0
    (internal-macroboard-row 2) => 0)

  (fact "is 1 for macroboard position 3, 4 and 5"
    (internal-macroboard-row 3) => 1
    (internal-macroboard-row 4) => 1
    (internal-macroboard-row 5) => 1)

  (fact "is 2 for macroboard position 6, 7 and 8"
    (internal-macroboard-row 6) => 2
    (internal-macroboard-row 7) => 2
    (internal-macroboard-row 8) => 2))

(facts "prepare a move for output"
  (fact "convert internal macroboard column to board column in macroboard 0"
    (internal-macro-col->board-col 0 0) => 0
    (internal-macro-col->board-col 0 1) => 1
    (internal-macro-col->board-col 0 2) => 2
    (internal-macro-col->board-col 0 3) => 0
    (internal-macro-col->board-col 0 4) => 1
    (internal-macro-col->board-col 0 5) => 2
    (internal-macro-col->board-col 0 6) => 0
    (internal-macro-col->board-col 0 7) => 1
    (internal-macro-col->board-col 0 8) => 2)

(fact "convert internal macroboard column to board column in macroboard 4"
    (internal-macro-col->board-col 4 0) => 3
    (internal-macro-col->board-col 4 1) => 4
    (internal-macro-col->board-col 4 2) => 5
    (internal-macro-col->board-col 4 3) => 3
    (internal-macro-col->board-col 4 4) => 4
    (internal-macro-col->board-col 4 5) => 5
    (internal-macro-col->board-col 4 6) => 3
    (internal-macro-col->board-col 4 7) => 4
    (internal-macro-col->board-col 4 8) => 5)

(fact "convert internal macroboard column to board column in macroboard 8"
    (internal-macro-col->board-col 8 0) => 6
    (internal-macro-col->board-col 8 1) => 7
    (internal-macro-col->board-col 8 2) => 8
    (internal-macro-col->board-col 8 3) => 6
    (internal-macro-col->board-col 8 4) => 7
    (internal-macro-col->board-col 8 5) => 8
    (internal-macro-col->board-col 8 6) => 6
    (internal-macro-col->board-col 8 7) => 7
    (internal-macro-col->board-col 8 8) => 8)

  (fact "convert internal macroboard row to board row in macroboard 0"
    (internal-macro-row->board-row 0 0) => 0
    (internal-macro-row->board-row 0 1) => 0
    (internal-macro-row->board-row 0 2) => 0
    (internal-macro-row->board-row 0 3) => 1
    (internal-macro-row->board-row 0 4) => 1
    (internal-macro-row->board-row 0 5) => 1
    (internal-macro-row->board-row 0 6) => 2
    (internal-macro-row->board-row 0 7) => 2
    (internal-macro-row->board-row 0 8) => 2)

  (fact "convert internal macroboard row to board row in macroboard 4"
    (internal-macro-row->board-row 4 0) => 3
    (internal-macro-row->board-row 4 1) => 3
    (internal-macro-row->board-row 4 2) => 3
    (internal-macro-row->board-row 4 3) => 4
    (internal-macro-row->board-row 4 4) => 4
    (internal-macro-row->board-row 4 5) => 4
    (internal-macro-row->board-row 4 6) => 5
    (internal-macro-row->board-row 4 7) => 5
    (internal-macro-row->board-row 4 8) => 5)

  (fact "convert internal macroboard row to board row in macroboard 8"
    (internal-macro-row->board-row 8 0) => 6
    (internal-macro-row->board-row 8 1) => 6
    (internal-macro-row->board-row 8 2) => 6
    (internal-macro-row->board-row 8 3) => 7
    (internal-macro-row->board-row 8 4) => 7
    (internal-macro-row->board-row 8 5) => 7
    (internal-macro-row->board-row 8 6) => 8
    (internal-macro-row->board-row 8 7) => 8
    (internal-macro-row->board-row 8 8) => 8)

  (fact "convert move for output"
    (convert-move-for-output 0 0) => '(0 0)
    (convert-move-for-output 0 1) => '(1 0)
    (convert-move-for-output 0 2) => '(2 0)
    (convert-move-for-output 4 6) => '(3 5)
    (convert-move-for-output 4 8) => '(5 5)
    (convert-move-for-output 8 3) => '(6 7)
    (convert-move-for-output 8 8) => '(8 8)))
