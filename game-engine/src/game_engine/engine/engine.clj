(ns game-engine.engine.engine
  "Controls the flow of the game engine"
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                  Validation Helpers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; (output-updates state-map) -> state-map?
; state-map -> map? of game-state
(defn output-updates
  "Write results from the round to the wherever they're supposed to go.
  Returns the same state-map that was passed in so this function can 
  be chained."
  [state-map]
  ; (println (:moving-player state-map) "returns:" (:move-input state-map))
  (println "Move" (:move state-map) state-map)
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
        (update-with-input input-str)
        (update-state-field-vector)
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
             (< (:move state-map) 11))
          (let [bot-chan ((:moving-player state-map) bot-channels)] 
            ; update the bot with current state-map
            (send-bot-updates bot-chan state-map)
            
            ; prompt the bot to move
            (>!! bot-chan "action move now")
            
            ; wait for response 
            (recur (update-game-state state-map (<!! my-chan))))
          ; else
          (println "Error!" state-map)))))
