(ns basicbot.board.board-math
  "Functions for calculating board positions and converting
  between board positions")

; (upper-left-macro-column macro_num) -> int?
; macro_num -> int?
(defn upper-left-macro-column
  "Return the upper left column of a macroboard
  This returns where on the big board interal column 0 lies"
  [macro_num]
   (+ (mod macro_num 3) (* 2 (mod macro_num 3))))

; (upper-left-macro-row macro_num) -> int?
; macro_num -> int?
(defn upper-left-macro-row
  "Return the upper left row of a macroboard.
  This returns where on the big board internal macroboard row 0 lies"
  [macro_num]
  (+ (quot macro_num 3) (* 2 (quot macro_num 3))))

; (internal-macroboard-column index)
; index -> int? index within a macroboard tile
(defn internal-macroboard-column
  "Return the column number from 0 to 2 that an index of 0 to 8
  would belong to within a macroboard. This is only for when we
  consider the moves inside a macroboard"
  [index]
  (mod index 3))

; (internal-board-row index) -> int?
; index -> int? index within a macroboard tile
(defn internal-macroboard-row
  "Return the row number from 0 to 2 that an index of 0 to 8
  would belong to within a macroboard. This is only for when we
  consider the moves inside a macroboard"
  [index]
  ; quot is the quotient function, which is the same as floor
  (quot index 3))

; (internal-macro-col->board-col macro-num index) -> int?
; macro-num -> int? index of macroboard
; index -> int? index within a macroboard
(defn internal-macro-col->board-col
  "Translate an internal macroboard column to a column in the big board"
  [macro-num index]
  (+ (upper-left-macro-column macro-num) (internal-macroboard-column index)))

; (internal-macro-row->board-row macro-num index) -> int?
; macro-num -> int? index of macroboard
; index -> int? index within a macroboard
(defn internal-macro-row->board-row
  "Translate an internal macroboard row to a row in the big board" 
  [macro-num index]
  (+ (upper-left-macro-row macro-num) (internal-macroboard-row index)))

; (field-index)
; row -> int?
; col -> int?
(defn field-index
  "Return the index from 0 to 80 given row and column numbers"
  [row col]
  (+ (* 9 row) col))

; (next-row-same-cell)
; crnt-field-cell -> int?
(defn next-row-same-cell
  "returns the same cell in the next row for a given field index"
  [crnt-field-cell]
  (let [next-row-field-cell (+ crnt-field-cell 9)] next-row-field-cell))


