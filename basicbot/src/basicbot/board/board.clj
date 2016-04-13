(ns basicbot.board.board
  "Runctions for manipulating the game board"
  (:require [basicbot.board.board-math :refer :all]
            [clojure.string :as str]))

;;; These atoms hold values that may need to be accessed by
;;; more than one function or namespace
(def our-bot-id (atom "0"))
(def opponent-bot-id (atom "0"))
(def macroboard-list (atom (list)))
(def fields-list (atom (list)))


(defn determine-opponent-id
  "The game server only tells us what our bot id is. The only
  possible ids are '1' or '2'. Given our bot id, return the
  opponent id"
  [id-str]
  (if (= id-str "1")
    "2"
    "1"))

(defn game-input-starts-with-settings 
  "Input from the game server that starts with 'settings' is
  intended to update static information about the game such
  as bot ids. Update the static variables that hold these
  values give a vector created by splitting the input string.
  Currently we are only interested in bot-id settings"
  [v]
  (cond
    ; settings input will look like: "settings your_botid i"
    (= (v 1) "your_botid")
    (let [my-id (v 2)
          opponent-id (determine-opponent-id (v 2))]
      (swap! our-bot-id (fn [current_state] my-id))
      (swap! opponent-bot-id (fn [current_state] opponent-id)))))

; (available-for-move? idx arg) -> false or int?
; idx -> int?
; arg -> string?
(defn available-for-move?
  "Helper function for determining which macroboard(s) a move can be
  made in. A value of -1 in the macroboard list indicates a move
  can be made in the corresponding macroboard. Given an index of an
  argument in a seq and the value of the argument, return the index if
  the arg = -1, otherwise return false."
  [idx arg]
  (if (= arg "-1")
    idx
    false))

; (string-to-vector str rgx) -> vector? of string?
; str -> string?
; rgx -> regex expression to split string on
; vectors will work perfectly for us since the boards are 0 based
(defn string->vector
  "Split a string on a regular expression, return a vector of the results"
  [str rgx]
  (str/split str rgx))

; (macroboard-move-list str) -> list? of false? or int?
; str -> string? representing the macroboard
(defn macroboard-move-list
  "Return a list which helps in determining in which macroboards a move can be made. 
  The value at each position is false if the macroboard tile is not available for
  a move. The value is an integer corresponding to the macroboard position if
  the macroboard is available for a move"
  [str]
  ; map-indexed gives us the item and it's index within a seq
  (map-indexed available-for-move? (string->vector str #",")))

; (big-squares-available str) -> list?
; str -> string?
(defn big-squares-available
  "Return a list of macorboard squares that a move can be made in"
  [str]
  (let [lst (macroboard-move-list str)]
    ; macroboard-move-list is a list of false and/or integers
    ; remove false values from the list, leaving us with macroboard tile numbers
    (filter #(not (= false %)) lst)))

; (macro-get-top-row)
; macro-board-num -> int?
(defn macro-get-top-row
  "returns the top row contents within a given macroboard"
  [macro-board-num]
  (let [upper-left-cell (field-index (upper-left-macro-row macro-board-num) 
                                     (upper-left-macro-column macro-board-num))]
    (let [upper-mid-cell (+ upper-left-cell 1)]
      (let [upper-right-cell (+ upper-left-cell 2)] 
        (let [top-row (list upper-left-cell 
                            upper-mid-cell 
                            upper-right-cell)]
          top-row)))))

; (parse-macro-board)
; mb-num -> int?
(defn parse-macro-board
  "Given a macroboard number returns a list of values from 
  left to right top to bottom"
  [mb-num]
  (cond 
    (and (>= mb-num 0) (<= mb-num 8))
    (let [top-row (macro-get-top-row mb-num)]
      (let [mid-row (map next-row-same-cell (into [] top-row))]
        (let [bottom-row (map next-row-same-cell (into [] mid-row))]
          (flatten (list top-row mid-row bottom-row)))))
    :else (list 0)))

; (macro-board-cell-available?)
; idx -> int?
; val -> int?
(defn macro-board-cell-available? 
  "returns the idx provided if val is 0, 
  indicating an avilable cell, or false otherwise"
  [idx val]
  (if (= val 0) idx false))

; (macro-board-available-cells)
; mb-cells -> list? of int?
; field -> list? of int?, The complete playing field in the current game state
(defn macro-board-available-cells
  "returns the list of indices of available cells inside the macroboard"
  [mb-cells field]
  ; (nth field (nth mb-cells 3))
  (let [mb-cell-values (map (fn [idx] (nth field idx)) mb-cells)]
    (let [mb-cells-available (map macro-board-cell-available? (range 9) mb-cell-values)]
      (filter #(not (= false %)) mb-cells-available))))


