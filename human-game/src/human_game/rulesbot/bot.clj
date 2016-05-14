(ns human-game.rulesbot.bot
 (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go go-loop chan buffer close!]]
           [clara.rules :refer :all]
           [clara.tools.tracing :refer :all]
           [clara.tools.inspect :refer :all]))

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

(defn get-forward-diag-sum
  [nine-cells]
  )

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
(def macroboard-number (atom "0"))
(def current-macroboard (atom []))
(def in-chan (atom ""))
(def out-chan (atom ""))

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
  ;; (when logflag (log "game-input-starts-with-update called with:" v))
  (cond
    (and (= (v 0) "game")
         (= (v 1) "field"))
    (swap! field-vector (fn [current_state] (string->vector (v 2) #",")))
    (and (= (v 0) "game")
         (= (v 1) "macroboard"))
    (swap! macroboard-vector (fn [current_state] (string->vector (v 2) #",")))
    (and (= (v 0) "game")
         (= (v 1) "round"))
    (swap! round-number (fn [current_state] (v 2)))
    (and (= (v 0) "game")
         (= (v 1) "move"))
    (swap! move-number (fn [current_state] (v 2)))))

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
          (flatten (vector top-row mid-row bottom-row)))))
    :else (vector 0)))

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
  (str "place_move " 
       (internal-macro-col->board-col macro-num index)
       " " 
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
      (vector macro-num move))))

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
(defn read-input-deprecated
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

;; =========================================
;; Records and Rules
;; =========================================

;; States
;; :1 - Initial
;; :2 - Select Macroboard
;; :3 - Select Move
;; :4 - Make Move
;; :100 - End

(defrecord State [state])

(defrecord Input [type args])

(defrecord Field [populated])

(defrecord Macroboard [populated])

(defrecord Move [populated])

;; Board/Field
;; Facts
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

(defrule get-input
  ""
  [State (= :1 state)]
  =>
  ;; (println "State is: Get Input.\n")
  (let [line (<!! @in-chan)
        line-vector (string->vector (clojure.string/trim line) #" ")]
    (insert! (->Input (first line-vector) (into [] (rest line-vector))))))

(defrule input-was-update
  ""
  [Input (= "update" type)]
  [?type <- Input (= "update" type)]
  =>
  (let [args (get ?type :args)]
    ;; (println "State is: Input Was Update.\n")
    (if (> (count args) 2)
      (cond
       (and (= (args 0) "game")
            (= (args 1) "field"))
       (do (swap! field-vector (fn [current_state] (string->vector (args 2) #",")))
           (if field-vector
             (insert! (->Field true))))
       (and (= (args 0) "game")
            (= (args 1) "macroboard"))
       (do (swap! macroboard-vector (fn [current_state] (string->vector (args 2) #",")))
           (insert! (->Macroboard true)))
       (and (= (args 0) "game")
            (= (args 1) "round"))
       (swap! round-number (fn [current_state] (args 2)))
       (and (= (args 0) "game")
            (= (args 1) "move"))
       (swap! move-number (fn [current_state] (args 2))))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule input-was-settings
  [Input (= "settings" type)]
  [?type <- Input (= "settings" type)]
  =>
  (let [args (get ?type :args)]
    ;; (println "State is: Input Was Settings.")
    (println args)
    (cond
    (= (args 0) "your_botid")
    (let [my-id (args 1)
          opponent-id (determine-opponent-id (args 1))]
      (swap! our-bot-id (fn [current_state] my-id))
      (swap! opponent-bot-id (fn [current_state] opponent-id)))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule input-was-action
  [Input (= "action" type)]
  [?type <- Input (= "action" type)]
  =>
  (let [args (get ?type :args)]
    ;; (println "State is: Input Was Action.\n")
    (cond
     (= (args 0) "move")
     (do (insert! (->State :2))))))

(defrule select-macroboard
  [State (= :2 state)]
  =>
  ;; (println "State is: Select Macroboard.\n")
    (if (not (empty? @macroboard-vector))
    (do (swap! macroboard-number (fn [current_state] (pick-move (big-squares-available @macroboard-vector))))
        ;; (println @macroboard-number)
        (swap! current-macroboard (fn [current_state] (into [] (parse-macro-board @macroboard-number))))
        ;; (println @current-macroboard)
        ))
  ;; (retract! (->State :3))
  (insert! (->State :3)))

(defrule check-for-opponent-potential-win
  [State (= :3 state)]
  =>
  ;; (println "State is: Check for Opponent Potential Win.\n")
  (let [forwardDiagSum (+ (read-string (@field-vector (@current-macroboard 0)))
                            (read-string (@field-vector (@current-macroboard 4)))
                            (read-string (@field-vector (@current-macroboard 8))))
        reverseDiagSum (+ (read-string (@field-vector (@current-macroboard 2)))
                            (read-string (@field-vector (@current-macroboard 4)))
                            (read-string (@field-vector (@current-macroboard 6))))
        firstColSum (+ (read-string (@field-vector (@current-macroboard 0)))
                         (read-string (@field-vector (@current-macroboard 3)))
                         (read-string (@field-vector (@current-macroboard 6))))
        secondColSum (+ (read-string (@field-vector (@current-macroboard 1)))
                          (read-string (@field-vector (@current-macroboard 4)))
                          (read-string (@field-vector (@current-macroboard 7))))
        lastColSum (+ (read-string (@field-vector (@current-macroboard 2)))
                        (read-string (@field-vector (@current-macroboard 5)))
                        (read-string (@field-vector (@current-macroboard 8))))
        firstRowSum (+ (read-string (@field-vector (@current-macroboard 0)))
                         (read-string (@field-vector (@current-macroboard 1)))
                         (read-string (@field-vector (@current-macroboard 2))))
        secondRowSum (+ (read-string (@field-vector (@current-macroboard 3)))
                          (read-string (@field-vector (@current-macroboard 4)))
                          (read-string (@field-vector (@current-macroboard 5))))
        lastRowSum (+ (read-string (@field-vector (@current-macroboard 6)))
                        (read-string (@field-vector (@current-macroboard 7)))
                        (read-string (@field-vector (@current-macroboard 8))))
        win-sum (* 2 (read-string @opponent-bot-id))]
    (cond (=  win-sum forwardDiagSum)
          (insert! (->MbForwardDiagSum forwardDiagSum))
          (= win-sum reverseDiagSum)
          (insert! (->MbReverseDiagSum reverseDiagSum))
          (= win-sum firstColSum)
          (insert! (->MbFirstColSum firstColSum))
          (= win-sum secondColSum)
          (insert! (->MbSecondColSum secondColSum))
          (= win-sum lastColSum)
          (insert! (->MbLastColSum lastColSum))
          (= win-sum firstRowSum)
          (insert! (->MbFirstRowSum firstRowSum))
          (= win-sum secondRowSum)
          (insert! (->MbSecondRowSum secondRowSum))
          (= win-sum lastRowSum)
          (insert! (->MbLastRowSum lastRowSum))
          :else (insert! (->State :4)))))

(defrule select-random-move
  [State (= :4 state)]
  =>
  ;; (println "State is: Select Random Move.\n")
  ;; (println "Select Move --Code goes here--.")
  (let [move-index ((choose-move-stage @macroboard-number @field-vector) 1)]
    (>!! @out-chan (convert-move-for-output @macroboard-number move-index)))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule input-was-end
  ""
  [Input (= "end" type)]
  =>
  ;; (println "State is: End.\n")
  (insert! (->State :100)))

;; ;; =================================
;; ;; Block Game Wins TODO: Block game wins
;; ;; =================================
;; (defrule block-forward-diag-game-win
;;   "if the forward diagonal sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardForwardDiagSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the forward diag win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-reverse-diag-game-win
;;   "if the reverse diagonal sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardReverseDiagSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the reverse diag win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-first-col-game-win
;;   "if the first col sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardFirstColSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the first col win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-second-col-game-win
;;   "if the second col sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardSecondColSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the second col win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-last-col-game-win
;;   "if the last col sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardLastColSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the last col win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-first-row-game-win
;;   "if the first row sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardFirstRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the first row win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-second-row-game-win
;;   "if the second row sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardSecondRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the second row win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; (defrule block-last-row-game-win
;;   "if the last row sum is twice the opponent
;;   id sum, place a move to block the win"
;;   [BoardLastRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
;;   =>
;;   (println "Blocking the last row win!")
;;   (retract! (->State :1))
;;   (insert! (->State :1)))

;; =================================
;; Block Macroboard Wins
;; =================================
(defrule block-forward-diag-mb-win
  "if the forward diagonal sum is twice the opponent
  id sum, place a move to block the win"
  [MbForwardDiagSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the forward diag win!")
  (cond (= (@field-vector (@current-macroboard 0)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 0)))
        (= (@field-vector (@current-macroboard 4)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 4)))
        (= (@field-vector (@current-macroboard 8)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 8))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-reverse-diag-mb-win
  "if the reverse diagonal sum is twice the opponent
  id sum, place a move to block the win"
  [MbReverseDiagSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the reverse diag win!")
  (cond (= (@field-vector (@current-macroboard 2)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 2)))
        (= (@field-vector (@current-macroboard 4)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 4)))
        (= (@field-vector (@current-macroboard 6)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 6))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-first-col-mb-win
  "if the first col sum is twice the opponent
  id sum, place a move to block the win"
  [MbFirstColSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the first col win!")
   (cond (= (@field-vector (@current-macroboard 0)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 0)))
        (= (@field-vector (@current-macroboard 3)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 3)))
        (= (@field-vector (@current-macroboard 6)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 6))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-second-col-mb-win
  "if the second col sum is twice the opponent
  id sum, place a move to block the win"
  [MbSecondColSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the second col win!")
   (cond (= (@field-vector (@current-macroboard 1)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 1)))
        (= (@field-vector (@current-macroboard 4)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 4)))
        (= (@field-vector (@current-macroboard 7)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 7))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-last-col-mb-win
  "if the last col sum is twice the opponent
  id sum, place a move to block the win"
  [MbLastColSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the last col win!")
   (cond (= (@field-vector (@current-macroboard 2)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 2)))
        (= (@field-vector (@current-macroboard 5)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 5)))
        (= (@field-vector (@current-macroboard 8)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 8))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-first-row-mb-win
  "if the first row sum is twice the opponent
  id sum, place a move to block the win"
  [MbFirstRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the first row win!")
   (cond (= (@field-vector (@current-macroboard 0)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 0)))
        (= (@field-vector (@current-macroboard 1)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 1)))
        (= (@field-vector (@current-macroboard 2)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 2))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-second-row-mb-win
  "if the second row sum is twice the opponent
  id sum, place a move to block the win"
  [MbSecondRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the second row win!")
   (cond (= (@field-vector (@current-macroboard 3)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 3)))
        (= (@field-vector (@current-macroboard 4)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 4)))
        (= (@field-vector (@current-macroboard 5)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 5))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule block-last-row-mb-win
  "if the last row sum is twice the opponent
  id sum, place a move to block the win"
  [MbLastRowSum (= sum (* 2 (read-string @opponent-bot-id)))]
  =>
  ;; (println "Blocking the last row win!")
   (cond (= (@field-vector (@current-macroboard 6)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 6)))
        (= (@field-vector (@current-macroboard 7)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 7)))
        (= (@field-vector (@current-macroboard 8)) "0")
        (do (>!! @out-chan (convert-move-for-output @macroboard-number 8))))
  (retract! (->State :1))
  (insert! (->State :1)))

(defrule end-game
  ""
  [State (= :100 state)]
  =>
  (println "Gameover. Thank you for playing.\n"))

; =========================================
; Main
; =========================================
(defn read-input 
  ""
  [read-chan write-chan]     
  (swap! in-chan (fn [current_state] read-chan))
  (swap! out-chan (fn [current_state] write-chan))
  (go 
    (defsession ttt-session 'human-game.rulesbot.bot)
    (-> ttt-session (insert (->State :1))
        (with-tracing)
        (fire-rules)
        (explain-activations))))
