(ns game-engine.engine.engine
  "Controls the flow of the game engine"
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                 Board Math 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (next-row-same-cell)
; crnt-field-cell -> int?
(defn next-row-same-cell
  "returns the same cell in the next row for a given field index"
  [crnt-field-cell]
  (let [next-row-field-cell (+ crnt-field-cell 9)] next-row-field-cell))

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


; (col-row->field-index [col-str row-str]) -> int?
; OR
; (col-row->field-index col row) -> int?
; argument should be a seq with at least two members
; col-str -> str? representing a column # from 0 to 8
; row-str -> str? representing a row # from 0 to 8 
; OR
; col -> int?
; row -> int?
(defn col-row->field-index
  "Return the index in the field vector from 0 to 80 that
  matches the position in the 9 x 9 representation of the board

  Note that this is a multi-arity function"
  ([[col-str row-str]]
   ; read-string converts a string to an int
   (+ (* 9 (read-string row-str)) (read-string col-str)))
  ([col row]
   (+ (* 9 row) col)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Game Running Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (build-initial-state) -> map?
(defn build-initial-state
  "The game engine tracks the game state with a map.
  Returns a map representing the initial game state."
  []
  (-> {}
      (assoc :field-vector (vec (repeat  81 "0")))
      (assoc :macroboard-vector (vec (repeat 9 "-1")))
      ; move is number from 0 to 80
      (assoc :move 0)
      ; moving player's requested move as string
      (assoc :move-input "")
      ; column the moving player wants to move to
      (assoc :move-col -1)
      ; row the moving player wants to move to
      (assoc :move-row -1)
      ; index of the macroboard the move was made in
      (assoc :macroboard-move-index -1)
      (assoc :game-winner false)
      (assoc :moving-player :bot1)))

(defn build-error-state
  [state-map input-str]
  (assoc state-map  
         :error "invalid move requested"
         :invalid-move input-str))

; (flip-player bot) -> keyword?
; bot -> keyword? (value should be :bot1 or :bot2)
(defn flip-player
  "Return the opposite of the bot keyword passed in. For example,
  an input of :bot1 should return a result of :bot2"
  [bot]
  (if (= bot :bot1)
    :bot2
    :bot1))

; (bot-number bot-id) -> int?
; bot-id -> keyword? 
(defn bot-number
  "Return the integer only bot id"
  [bot-id]
  (if (= bot-id :bot1)
    1
    2))

(defn build-helper-macro-vector
  "Return a vector of all nil except '-1' at the index
  = required-move"
  [required-move]
  (assoc (vec (repeat 9 nil)) required-move "-1"))

(defn compare-transform-args 
  "Helper for transforming macroboard"
  [x y]
  (if (= x y)
    x
    (if (= "-1" x)
      "0"
      x)))

; (macroboard-index-from move col row) -> int?
; col -> int?
; row -> int?
(defn macroboard-index-from-move
  "Return the internal index of a move so we know where an opponent's
  next move must be"
  [col row]
  (+ (* 3 (rem row 3)) (rem col 3)))

; (transform-macroboard-output state-map) -> macroboard-vector?
; state-map-> map? of game-state 
(defn transform-macroboard-output
  "In the event that a player's move forces their opponent to play in a
  specific macroboard, the output should show 0's for any position not
  won by either player. Yes, this is confusing because 0 also means draw,
  but that's how the api does it"
  [state-map]
  ; need to check for -1 to make sure this isn't the initial loop of the game
  ; which would cause the game to crash
  (if (not (= -1 (:macroboard-move-index state-map)))
    (let [required-macroboard-move (macroboard-index-from-move (:move-col state-map) (:move-row state-map))]  
      (if (= "-1" ((:macroboard-vector state-map) required-macroboard-move))
        ; transform output
        (into [] (map compare-transform-args (:macroboard-vector state-map) (build-helper-macro-vector required-macroboard-move)))
        ; else nothing to do, return the macroboard as is
        (:macroboard-vector state-map)))
    (:macroboard-vector state-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Validation Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn within-range?
  "Return true if the string x between 0 and 8 inclusive"
  [x]
  (let [x-int (read-string x)]
    (and (>= x-int 0) (<= x-int 8))))

(defn int-within-range?
  "Return true if str is an integer of length 1 (between 0 and 9)"
  [str]
  (if (= 1 (count str))
    (when (integer? (read-string str))
      (within-range? str))
    false))

; (col-row->macroboard col row) -> int?
; col -> int?
; row -> int?
(defn col-row->macroboard
  "Take the big board column and row and return the macroboard
  they belong to"
  [col row]
  (let [macro-row (quot row 3)
        macro-col (quot col 3)]
    (+ (* macro-row 3) macro-col)))

; (input-valid? input-vec) -> bool?
; input-vec -> vector? of string?
(defn input-valid?
  [input-vec]
  ; starts with place_move
  ; split vector has length 3
  ; 2nd and 3rd values are int
  ; 2nd and 3rd values are between 0 and 8 inclusive
  (let [validation-vec [#(= "place_move" (first %))
                        #(= 3 (count %))
                        #(every? int-within-range? (rest %))]]
    (every? true? (map #(% input-vec) validation-vec))))

; (macroboard-placement-valid? col row macroboard-vector) -> bool?
; col -> int?
; row -> int?
; macroboard-vector -> vector? of int?
(defn macroboard-placement-valid? 
  "Return true if col and row are in an
  open macroboard"
  [macroboard-vector col row]
  ; macroboard is vector of strings
  (= "-1" (macroboard-vector (col-row->macroboard col row))))

; (field-placement-valid? field-vector col row) -> bool?
; field-vector -> vector? of string?
; col -> int?
; row -> int? 
(defn field-placement-valid? 
  "Return true if the position of col and row in the field-vector
 is open for a move (value = '0')"
  [field-vector col row]
  (= "0" (field-vector (col-row->field-index col row))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Win Checking Helpers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (build-row-list board-vector move-row) -> list?
(defn build-row-list
  "Return the list of values in the row of a board list"
  [board-vector move-row]
  (nth (partition 3 board-vector) move-row))

; (build-column-list board-vector move-col) -> list?
(defn build-column-list
  "Return the list of values in the column of a board list"
  [board-vector move-col]
  (nth (apply map #(list %1 %2 %3) (partition 3 board-vector)) move-col))

; (take-indices take-vec idx-list) -> list?
; take-vec -> vector? of any type of value
; idx-list -> list? of int? 
(defn take-indices
  "Take values from take-vec at the indices specified in idx-list"
  [take-vec idx-list]
  (map #(take-vec %) idx-list))

; (build-left-diagonal board-vector) -> list?
(defn build-left-diagonal
  "Return the diagonal in the board list starting in the upper left corner"
  [board-vector]
  (take-indices board-vector '(0 4 8)))

; (build-right-diagonal board-vector) -> list?
(defn build-right-diagonal
  "Return the diagonal in the board list starting in the upper right corner"
  [board-vector]
  (take-indices board-vector '(2 4 6)))

(defn build-check-list
  "Return a list of lists representing the rows columns
  and diagonals that need to be checked in board-vector"
  [board-vector move-col move-row]
  (let [idx (+ (* 3 move-row) move-col)]
    ; cond->> threads the value as the last argument of the
    ; function only if the conditional evaluates to true,
    ; otherwise it just moves on to the next line
    (cond->> '()
      ; will always be a column and row to return
      true (cons (build-row-list board-vector move-row))
      true (cons (build-column-list board-vector move-col))
      (= idx 0) (cons (build-left-diagonal board-vector))
      (= idx 2) (cons (build-right-diagonal board-vector))
      (= idx 4) (cons (build-left-diagonal board-vector))
      (= idx 4) (cons (build-right-diagonal board-vector))
      (= idx 6) (cons (build-right-diagonal board-vector))
      (= idx 8) (cons (build-left-diagonal board-vector)))))

; (board-won check-list player-str) -> bool?
; check-list -> list? of column, row and diagonal lists from a board
; player-str -> string? representing player number
(defn board-won?
  "Return true if player-str has won and of the lists
  in check-list. This would mean the player has won a
  board"
  [check-list player-str]
  ; cons the player str with each list in check-list
  ; explode the resulting list so we can pass the values
  ; as arguments to '='
  ; return true if any of the lists in check-list contain
  ; only values = player-str
  (if (some #(apply = (cons player-str %)) check-list)
    true
    false))

; (macro-get-top-row)
; macro-board-num -> int?
(defn macro-get-top-row
  "returns the top row contents within a given macroboard"
  [macro-board-num]
  (let [upper-left-cell (col-row->field-index (upper-left-macro-column macro-board-num) 
                                              (upper-left-macro-row macro-board-num))]
    (let [upper-mid-cell (+ upper-left-cell 1)]
      (let [upper-right-cell (+ upper-left-cell 2)] 
        (let [top-row (list upper-left-cell 
                            upper-mid-cell 
                            upper-right-cell)]
          top-row)))))

; (parse-macro-board)
; mb-num -> int?
(defn parse-macroboard
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


; (build-internal-macroboard field-vector mb-num) -> vector? of string?
; field-vector -> vector? of string?
; mb-num -> int? macroboard number to build from
(defn build-internal-macroboard
  "Get a vector of the values within a macroboard"
  [field-vector mb-num]
  ; parse-macroboard returns a list of field-vector indices for a macroboard
  (into [] (map #(field-vector %) (parse-macroboard mb-num))))

; (board-draw? board-vector) -> bool?
; board-vector -> vec? of string?
(defn board-draw?
  "Return true if the board is full"
  [board-vector]
  (every? true? (map #(not (= "0" %)) board-vector)))

; (defn check-macroboard-win state-map) -> state-map?
; state-map -> map? of game state
(defn check-macroboard-win
  "Return an updated state-map if the moving player has won a macroboard"
  [state-map]
  (let [board-vector (build-internal-macroboard (:field-vector state-map) (:macroboard-move-index state-map))
        check-list (build-check-list board-vector (mod (:move-col state-map) 3) (mod (:move-row state-map) 3))]
    (if (board-won? check-list (str (bot-number (:moving-player state-map))))
      ; update the macroboard vector the moving player's id
      (assoc-in state-map [:macroboard-vector (:macroboard-move-index state-map)] (str (bot-number (:moving-player state-map))))
      ; if no win, check for board draw
      (if (board-draw? board-vector)
        ; '0' indicates board-draw
        (assoc-in state-map [:macroboard-vector (:macroboard-move-index state-map)] "0")
        ; if no win or draw, return state-map as is
        state-map))))

; (check-game-win state-map) -> state-map?
; state-map -> map? of game state
(defn check-game-win
  "Check if the game has been won"
  [state-map]
  ; need a list of rows, columns and diagonals to check for a win
  (let [check-list (build-check-list (:macroboard-vector state-map) (internal-macroboard-column (:macroboard-move-index state-map)) (internal-macroboard-row (:macroboard-move-index state-map)))]
    (if (board-won? check-list (str (bot-number (:moving-player state-map))))
      ; update the game winner
      (assoc state-map :game-winner (:moving-player state-map))
      state-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Update Helpers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (parse-move input-str idx) -> int?
; input-str -> string? player's requested move as string
; idx -> int? which argument in the string to parse
(defn parse-move
  "Helper function for converting the col or row input string to
  an int"
  [input-str idx]
  (read-string ((clojure.string/split input-str #" ") idx)))

; (flip-stuff state-map) -> state-map?
; state-map? -> map? of game state
(defn flip-stuff
  "Helper function. Increment the move number and flip the moving player"
  [state-map]
  (assoc state-map :move (+ (:move state-map) 1) :moving-player (flip-player (:moving-player state-map))))

; (update-with-input state-map input-str) -> state-map?
; state-map -> map? of game state
; input-str -> string? player's move request
(defn update-with-input
  "Return a new state map with updated values for
  :move-input, :move-col, :move-row, :macroboard-move-index
  based on input-str"
  [state-map input-str]
  (let [col (parse-move input-str 1)
        row (parse-move input-str 2)
        macro-idx (col-row->macroboard col row)]
    (assoc state-map
           :move-input input-str
           :move-col col
           :move-row row
           :macroboard-move-index macro-idx)))

; (update-field-vector state-map) -> vector?
; state-map -> map? of game state
(defn update-field-vector
  "Update a value in a field-vector and return a new field vector"
  [state-map]
  (assoc (:field-vector state-map) 
         ; get the index we need to update in field vector
         (col-row->field-index (:move-col state-map) (:move-row state-map))
         ; get the player number we need to write to the field vector
         (str (bot-number (:moving-player state-map)))))

; (update-state-field-vector state-map) -> state-map?
; state-map -> map? of game state
(defn update-state-field-vector
  "Update the field vector in the state map and return a new state map"
  [state-map]
  (assoc state-map :field-vector (update-field-vector state-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           Engine Output 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn output-field
  [board]
  (let [board-rows (partition 9 board)]
    (doall (map println board-rows))))

(defn output-macroboard
  [board]
  (let [board-rows (partition 3 board)]
    (doall (map println board-rows))))

; (output-updates state-map) -> state-map?
; state-map -> map? of game-state
(defn output-updates
  "Write results from the round to the wherever they're supposed to go.
  Returns the same state-map that was passed in so this function can 
  be chained."
  [state-map]
  ; (println (:moving-player state-map) "returns:" (:move-input state-map))
  (println "Move #" (:move state-map))
  (println (:moving-player state-map) "makes move:" (:move-input state-map))
  (println "Board:")
  (output-field (:field-vector state-map))
  (println "Macroboard:")
  (output-macroboard (:macroboard-vector state-map))
  ; return state map otherwise bad things will happen
  state-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Engine Stages 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (validate-requested-move input-str macroboard-vector field-vector) -> bool?
; input-str -> string?
; macroboard-vector -> vector? of string?
; field-vector -> vector? of string?
(defn validate-requested-move
  "Confirm the requested move is in the right format
  and in an open board position"
  [input-str macroboard-vector field-vector]
  ; check that input is formatted correctly first
  (let [input-vec (clojure.string/split input-str #" ")]
      (if (input-valid? input-vec)
      ; if yes, make sure the actual move is valid
        (do
          (let [col (read-string (input-vec 1))
                row (read-string (input-vec 2))]
            (every? true? (list
                           (macroboard-placement-valid? macroboard-vector col row)
                           (field-placement-valid? field-vector col row)))))
          ; else return false
        false)))

; (send-bot-updates bot-chan state-map) -> nil
; bot-chan -> channel?
; state-map -> map? the game state map  
(defn send-bot-updates
  "Send game state updates to bot-chan so the bot has the information it
  needs to make a proper move"
  [bot-chan state-map]
  (>!! bot-chan (str "update game round " (+ (quot (:move state-map) 2) 1)))
  (>!! bot-chan (str "update game move " (+ (mod (:move state-map) 2) 1)))
  (>!! bot-chan (str "update game field " (clojure.string/join "," (:field-vector state-map))))
  (>!! bot-chan (str "update game macroboard " (clojure.string/join "," (transform-macroboard-output state-map)))))

(defn update-game-state 
  [state-map input-str]
  ; if input-str in correct format and requested move is an open move
  (if (validate-requested-move input-str (:macroboard-vector state-map) (:field-vector state-map))
    ; then update and return the game state
    (-> state-map
        ; update state based on the input string
        (update-with-input input-str)
        ; update the field vector with the user's move
        (update-state-field-vector)
        ; check if the user won the game inside a macroboard
        (check-macroboard-win)
        ; check if the user won the game
        (check-game-win)
        ; log the round's game state
        (output-updates)
        ; until I think of a better name, this is how we increment the move # and flip the moving player
        (flip-stuff))

    ; else return error state
    (build-error-state state-map input-str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;             Run Game!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (run-game my-chan bot1-chan bot2-chan) -> nil?
; my-chan -> channel?
; bot1-chan -> channel?
; bot2-chan -> channel?
(defn run-game 
  "Give 3 distinct channels, simulate a game between two bots
  using the channels for communication between the game engine
  and the bots"
  [my-chan bot1-chan bot2-chan]
  
  ; inform bots of their id number
  (>!! bot1-chan "settings your_botid 1")
  (>!! bot2-chan "settings your_botid 2")
  
  ; define a routing map to simplify the routing and looping logic
  ; the key of the map matches the value of the
  ; moving-player key in the state map
  (let [bot-channels {:bot1 bot1-chan :bot2 bot2-chan}]
      (loop [state-map (build-initial-state)]
        ; keep going if no game winner or < than 81 moves have been completed
        (if (and 
             (not (contains? state-map :error)) 
             (not (:game-winner state-map)))
          (let [bot-chan ((:moving-player state-map) bot-channels)] 
            ; update the bot with current state-map
            (send-bot-updates bot-chan state-map)
            
            ; prompt the bot to move
            (>!! bot-chan "action move now")
            
            ; wait for response 
            (recur (update-game-state state-map (<!! my-chan))))
          ; else
          (if (:game-winner state-map) 
            (println "Winner is" (:game-winner state-map) "!!!") 
            (println "Error! Game Aborted" state-map))))))
