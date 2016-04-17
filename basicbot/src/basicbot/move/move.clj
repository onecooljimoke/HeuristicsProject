(ns basicbot.move.move
  "Functions for selecting a move"
  (:require [basicbot.board.board :refer :all]))

; (pick-move)
; mb-available-cells -> int?
(defn pick-move
  "returns a valid index in the macroboard list if
  the list is non-empty, otherwise it returns -1"
  [mb-available-cells]
  (if (empty? mb-available-cells)
    -1
    (rand-nth mb-available-cells)))

; (macroboard-stage board-vec) -> int?
; board-vec -> vector? of string?
(defn macroboard-stage
  "As part of the process for selecting a move,
  given a vector representing the macroboard, determine
  which macroboard locations we can move in, choose a
  macroboard location, and output the result" 
  [board-vec]
  (pick-move (big-squares-available board-vec)))

; Hey, Nicole!  Put your damn function here!
(defn choose-move-stage
  [macro-num field]
  (let [cells-available 
        (macro-board-available-cells (parse-macro-board macro-num) field)]
    (let [move (pick-move cells-available)]
      (list macro-num move))))

; (output-string move-lst) -> string?
; move-lst -> list? of string?
(defn output-string
  "Return a string in the correct format for output to the game.
  Expects a list whose first item is the column number and whose
  second item is a row number"
  [move-lst]
  (str "place_move " (first move-lst) " " (second move-lst)))

; (output-stage macro-num macro-idx) -> string?
; input should be a list that contians macro-num and macro-idx
; in first two positions
; macro-num -> int?
; macro-idx -> int?
(defn output-stage
  "Given the index of a field within a macroboard,
  and the macroboard number, convert the location
  to it's coordinates on the big board and return
  a string in the proper format for output to the
  game server"
  [[macro-num macro-idx]]
  (output-string (convert-move-for-output macro-num macro-idx)))

; (game-input-starts-with-move) -> string?
(defn game-input-starts-with-move
  "Choose a move on the gameboard and return a string
  in the correct format for communicating a move to the
  game server"
  ; I put a rest argument here so function conforms with the 
  ; settings and actions signatures 
  ; This makes it easier to route by input in io.clj
  [& args]
  (->
   @macroboard-vector
   (macroboard-stage)
   (choose-move-stage @field-vector)
   (output-stage)))
