(ns rulesbot.core
  (:require [clara.rules :refer :all]
            [clara.tools.tracing :refer :all]
            [clara.tools.inspect :refer :all])
  (:gen-class))

; =========================================
; Logging functionality
; =========================================

(def logflag true)

(defn log
  "Write to standard error"
  [& args]
  (.println *err* args))

; =========================================
; Board Math Functions 
; Functions for calculating board positions 
; and converting between board positions
; =========================================

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


; =========================================
; Board Functions 
; Functions for mainpulating the game board
; =========================================

;;; These atoms hold values that may need to be accessed by
;;; more than one function or namespace
(def round-number (atom "0"))
(def move-number (atom "0"))
(def our-bot-id (atom "0"))
(def opponent-bot-id (atom "0"))
(def macroboard-vector(atom []))
(def field-vector(atom []))

; (determine-opponent-id id-str) -> string?
; id-str -> string
(defn determine-opponent-id
  "The game server only tells us what our bot id is. The only
  possible ids are '1' or '2'. Given our bot id, return the
  opponent id"
  [id-str]
  (if (= id-str "1")
    "2"
    "1"))

; (game-input-starts-with-settings v) -> nil
; v -> vector? of string?
(defn game-input-starts-with-settings 
  "Input from the game server that starts with 'settings' is
  intended to update static information about the game such
  as bot ids. Update the static variables that hold these
  values given a vector created by splitting the input string.
  Currently we are only interested in bot-id settings"
  [v]
  (when logflag (log "game-input-starts-with-settings called with:" v))
  (cond
    ; settings input will look like: "settings your_botid i"
    (= (v 1) "your_botid")
    (let [my-id (v 2)
          opponent-id (determine-opponent-id (v 2))]
      (swap! our-bot-id (fn [current_state] my-id))
      (swap! opponent-bot-id (fn [current_state] opponent-id))))
  ; return nil to make routing easier, we already know what the return value
  ; of swap is anyway
  nil)

; (string-to-vector str rgx) -> vector? of string?
; str -> string?
; rgx -> regex expression to split string on
; vectors will work perfectly for us since the boards are 0 based
(defn string->vector
  "Split a string on a regular expression, return a vector of the results"
  [str rgx]
  (clojure.string/split str rgx))

; (game-input-starts-with-update v) -> nil
; v -> vector? of string?
(defn game-input-starts-with-update
  "Input from the game server that starts with update is
  intended to update information that changes periodically
  throughout the game such as the field list and the macroboard
  list. Update the static variables that hold these values given
  a vector created by splitting the input string. Currently
  we're only interested in macroboard and field"
  [v]
  ; settings input will look like:
  ; "update game field <str>"
  ; "update game macroboard <str>
  (when logflag (log "game-input-starts-with-update called with:" v))
  (cond
    (and (= (v 1) "game")
         (= (v 2) "field"))
    (swap! field-vector (fn [current_state] (string->vector (v 3) #",")))
    (and (= (v 1) "game")
         (= (v 2) "macroboard"))
    (swap! macroboard-vector (fn [current_state] (string->vector (v 3) #",")))
    (and (= (v 1) "game")
         (= (v 2) "round"))
    (swap! round-number (fn [current_state] (v 3)))
    (and (= (v 1) "game")
         (= (v 2) "move"))
    (swap! move-number (fn [current_state] (v 3))))
  ; return nil to make routing easier, we already know what the return value 
  ; of swap is anyway 
  nil)

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
; v -> vector? of string? representing the macroboard
(defn macroboard-move-list
  "Return a list which helps in determining in which macroboards a move can be made. 
  The value at each position is false if the macroboard tile is not available for
  a move. The value is an integer corresponding to the macroboard position if
  the macroboard is available for a move"
  [v]
  ; map-indexed gives us the item and it's index within a seq
  (map-indexed available-for-move? v))

; (big-squares-available str) -> list?
; str -> string?
(defn big-squares-available
  "Return a list of macorboard squares that a move can be made in"
  [v]
  (let [lst (macroboard-move-list v)]
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
  (if (= val "0") idx false))

; (macro-board-available-cells)
; mb-cells -> list? of int? List of indices in field-vector where macroboard
;                           values can be found
; field -> list? of int?, The complete playing field in the current game state
(defn macro-board-available-cells
  "returns the list of indices of available cells inside the macroboard"
  [mb-cells field]
  ; (nth field (nth mb-cells 3))
  (let [mb-cell-values (map (fn [idx] (nth field idx)) mb-cells)]
    (let [mb-cells-available (map macro-board-cell-available? (range 9) mb-cell-values)]
      (filter #(not (= false %)) mb-cells-available))))

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

; (moves-available?)
(defn moves-available?
  []
  (contains? @field-vector 0))

; =========================================
; Movement Functions 
; =========================================

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

; (choose-move-stage)
; macro-num -> int?
; field -> list? of int?
(defn choose-move-stage
  "Given a macroboard number, macro-num, and the current 
  field list, chooses an index inside that macroboard to 
  be the next move location. Outputs a list containing
  the macroboard number and the move (index in the 
  macroboard)."
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

; (game-input-starts-with-action) -> string?
(defn game-input-starts-with-action
  "Choose a move on the gameboard and return a string
  in the correct format for communicating a move to the
  game server"
  ; I put a rest argument here so function conforms with the 
  ; settings and actions signatures 
  ; This makes it easier to route by input in io.clj
  [& args]
  (when logflag
    (log "game-input-starts-with-action called"
         "game round:" @round-number 
         "game-move:" @move-number
         "current values:"
         "field:" @field-vector
         "macroboard:" @macroboard-vector))
  (->
   @macroboard-vector
   (macroboard-stage)
   (choose-move-stage @field-vector)
   (output-stage)))

; =========================================
; IO Functions 
; =========================================

; input-routes -> map?
(def input-routes
  "A map for referencing functions to call based on the
  first word of the game input string. The keys are the
  potential first words, the values are the functions to
  call."
  {"settings" game-input-starts-with-settings 
   "update" game-input-starts-with-update 
   "action" game-input-starts-with-action})


; (route-by-input-type v) -> nil?
; v -> vector? of string?
(defn route-by-input-type
  "Given a vector of strings, v, call a function in input-routes based
  on the first string in v"
  [v]
  (let [type (v 0)]
    ; make sure v is a key in input-routes
    (if (contains? input-routes type)
      ((input-routes type) v)
      (do
        (log "Error, can't find route:" type)
        ; explicitly return nil just to be safe
        ; the default behavior of any route that's not action is
        ; to return nil
        nil))))

;(read-input)
; listen to standard-input and write it to standard output
(defn read-input
  "Use java.io.BufferedReader and .BufferedWriter to read
  continuous user input

  Stop listening by typing 'end' "
  []
  (let [rdr (java.io.BufferedReader. *in*)]
    (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
      (when logflag (log "input is:" ln))
      (if (not (empty? ln))
        (let [output (route-by-input-type (string->vector (clojure.string/trim ln) #" "))]
          (when logflag (log "chosen move:" output))
          ; output is either nil or a string in the right format for
          ; outputting a move
          (if output
            (println output)))))
    ; close rdr because we're considerate programmers
    ;; (.close rdr)
    ))

; =========================================
; Records and Rules
; =========================================
; available states are:
;  [1]  - 
(defrecord State [state])  ; This may be unnecessary

;; Board/Field Facts
(defrecord BoardForwardDiagSum [sum])
(defrecord BoardReverseDiagSum [sum])
(defrecord BoardFirstColSum [sum])
(defrecord BoardSecondColSum [sum])
(defrecord BoardLastColSum [sum])
(defrecord BoardFirstRowSum [sum])
(defrecord BoardSecondRowSum [sum])
(defrecord BoardLastRowSum [sum])

;; Macroboard Facts
(defrecord MbForwardDiagSum [sum])
(defrecord MbReverseDiagSum [sum])
(defrecord MbFirstColSum [sum])
(defrecord MbSecondColSum [sum])
(defrecord MbLastColSum [sum])
(defrecord MbFirstRowSum [sum])
(defrecord MbSecondRowSum [sum])
(defrecord MbLastRowSum [sum])



; (init-game)
(defrule init-game
  "requires the initial state 1, and changes to the next
  state which is to begin reading input"
  {:salience 500}
  [State (= :1 ?state)]
  =>
  ;; (insert! (->State 2))

  ;; (insert! (->TakeInput false))
  (println ?state))

(defrule get-input
  ""
  {:salience 475}
  [State (= :1 ?state)]
  =>
  ;; (insert! (->State 4))
  ;; (insert! (->TakeInput true))
  (let [line (.readLine (java.io.BufferedReader. *in*))]
    (println line))
  (insert! (->BoardReverseDiagSum (* 2 (read-string @opponent-bot-id)))))

;; (defrule check-moves-available
;;   ""
;;   {:salience 400}
;;   [State (= ?state 3)]
;;   [TakeInput (= ?done true)]
;;   [:not (TakeInput (= ?done false))]
;;   =>
;;   (insert! (->State 4))
;;   (println moves-available?))

(defrule block-game-win
  "if any of the board/field sum facts are twice opponent
  id sum, place a move to block the win, in the case of
  the overall board may want to drive them away from the
  macroboard that would allow them to win"
  [:or [BoardForwardDiagSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardReverseDiagSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardFirstColSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardSecondColSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardLastColSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardFirstRowSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardSecondRowSum (= ?sum (* 2 (read-string @opponent-bot-id)))]
   [BoardLastRowSum (= ?sum (* 2 (read-string @opponent-bot-id)))]]
  =>
  (println "Blocking the win!")
  (insert! (->State :1)))

;; (defrule block-macroboard-win
;;   "if any of the macroboard sum facts are twice opponent
;;   id sum, place a move to block the win"
;;   {}
;;   [])

(defrule end-game
  ""
  {:salience -100}
  [State (= :2 ?state)]
  =>
  (println "Gameover. Thank you for playing."))

; =========================================
; Main
; =========================================
(defn -main
  ""
  [& args]
  (defsession ttt-session 'rulesbot.core)
  (-> ttt-session (insert (->State :1))
      (with-tracing)
      (fire-rules)
      (explain-activations)
      (println "HERE!!!!!")))
