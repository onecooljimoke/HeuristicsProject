(ns basicbot.core
  (require [clojure.string :as str])
  (:gen-class))

; input-routes -> map?
(def input-routes
  "key is potential first argument for input
  and value is function to call"
  {"settings" #(println "The type is: 'settings'")
   "move" #(println "The type is: 'move'")
   "action" #(println "The type is: 'action'")})

; (string-to-vector str rgx) -> vector? of string?
; str -> string?
; rgx -> regex expression to split string on
; vectors will work perfectly for us since the boards are 0 based
(defn string-to-vector
  "Split a str on rgx"
  [str rgx]
  (str/split str rgx))

; (route-by-input-type v) -> nil?
; v -> vector? of string?
(defn route-by-input-type
  "Call a function in input-routes based on the first item in v"
  [v]
  (let [type (v 0)]
    ; make sure v is a key in input-routes
    (if (contains? input-routes v)
      ((input-routes type))
      (println "Error: can't find: " v))))

; (available-for-move? idx arg) -> false or int?
; idx -> int?
; arg -> string?
(defn available-for-move?
  "Returns idx if arg = '-1', else returns false"
  [idx arg]
  (if (= arg "-1")
    idx
    false))

; (macroboard-move-list str) -> list? of false? or int?
; str -> string? representing the macroboard
(defn macroboard-move-list
  "Return a list where each item corresponds to the macroboard index.
  The value at each position is false if the macroboard tile is not available for
  a move or the position of the macroboard tile if the tile is available for
  a move"
  [str]
  ; map-indexed gives us the item and it's index in a seq
  (map-indexed available-for-move? (string-to-vector str #",")))

; (big-squares-available str) -> list?
; str -> string?
(defn big-squares-available
  "Return a list of macorboard squares that a move can be made in"
  [str]
  (let [lst (macroboard-move-list str)]
    ; remove false values from the list, leaving us with macroboard tile numbers
    (filter #(not (= false %)) lst)))

; (upper-left-macro-column macro_num) -> int?
; macro_num -> int?
(defn upper-left-macro-column
  "Return the upper left column of a macroboard"
  [macro_num]
   (+ (mod macro_num 3) (* 2 (mod macro_num 3))))

; (upper-left-macro-row macro_num) -> int?
; macro_num -> int?
(defn upper-left-macro-row
  "Return the upper left row of a macroboard"
  [macro_num]
  (+ (quot macro_num 3) (* 2 (quot macro_num 3))))

; (internal-macroboard-column index)
; index -> int? index within a macroboard tile
(defn internal-macroboard-column
  "Return the column number from 0 to 2 that an index of 0 to 8
  would belong to.  This is for when we only consider the moves
  inside a macroboard"
  [index]
  (mod index 3))

; (internal-board-row index) -> int?
; index -> int? index within a macroboard tile
(defn internal-macroboard-row
  "Return the row number from 0 to 2 that an index of 0 to 8
  would belong to.  This is for when we only consider the moves
  inside a macroboard"
  [index]
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

; (convert-move-for-output macro-num move-lst) -> list?
; macro-num -> int? index of the macroboard
; index -> int? index of move within the macroboard tile 
(defn convert-move-for-output
  "Convert a move inside a macroboard to a row and column in the big
  board so we can output the move to the game. The returned list
  holds the column first and then the row"
  [macro-num index]
  (list (internal-macro-col->board-col macro-num index)
        (internal-macro-row->board-row macro-num index)))

; (output-string move-lst) -> string?
; move-lst -> list? of string?
(defn output-string
  "Return a string in the correct format for output to the game"
  [move-lst]
  (str "place_move " (first move-lst) " " (second move-lst)))

;(read-input)
; listen to standard-input and write it to standard output
(defn read-input
  "Use java.io.BufferedReader and .BufferedWriter to read
  continuous user input

  Stop by typing 'end' "
  []
  (println "Now listening for input")
  (let [rdr (java.io.BufferedReader. *in*)
        wrt (java.io.BufferedWriter. *out*)]
    (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
      (.write wrt ln)
      ; .write doesn't print newlines
      (.newLine wrt)
      ; flush the buffer to output
      (.flush wrt))
    ; close rdr because we're considerate programmers
    ; closing wrt causes the program to crash
    (.close rdr)))

(defn field-index
  [row col]
  (+ (* 9 row) col))

; (parse-macro-board)
; mb-num -> int?
(defn parse-macro-board
  "Given a macroboard number returns a list of values from 
  left to right top to bottom"
  [mb-num]
  (def upper-left-cell (field-index (upper-left-macro-row mb-num) (upper-left-macro-column mb-num) ))
  (cond 
    (and (>= mb-num 0) (<= mb-num 8)) 
    (def board-list (list
        ; first three cells
        upper-left-cell (+ upper-left-cell 1) (+ upper-left-cell 2)
        ; next rows three cells
        (+ upper-left-cell 9) (+ upper-left-cell 10) (+ upper-left-cell 11)
        ; last rows three cells
        (+ upper-left-cell 18) (+ upper-left-cell 19) (+ upper-left-cell 20)))
    :else (def board-list '0))
        board-list)  ;; default - return empty board-list, ie, no cells for an invalid macroboard number

; (macro-board-available-cells)
; mb-cells -> list? of int?
; field -> list? of int?, The complete playing field in the current game state
(defn macro-board-available-cells
  [mb-cells field]
  ; (nth field (nth mb-cells 3))
  (def available-cells '())
  (loop [idx 8]
    (when (> idx -1)
      (def crnt-cell (nth field (nth mb-cells idx)))
      (cond (= crnt-cell 0)
            ; some shady business because "(conj available-cells crnt-cell)" wasn't working 
            (def available-cells (conj available-cells idx)))
      (recur (- idx 1)) ) )
  available-cells)

(defn -main
  ""
  [& args]
  ; (read-input)
  ; (println (parse-macro-board 0)) ;; print the resulting field cells from the given macroboard number
  ; (println (parse-macro-board -1))    ;; print the resulting field cells from the given invalid macroboard number
  ; (println (parse-macro-board 10))    ;; print the resulting field cells from the given invalid macroboard number


  (def mb (parse-macro-board 0))
  (def field '(1,2,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  (println mb)
  (println field)
  (println (macro-board-available-cells mb field))
)
