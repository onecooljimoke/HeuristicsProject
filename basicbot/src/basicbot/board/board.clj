(ns basicbot.board.board)

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

