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


