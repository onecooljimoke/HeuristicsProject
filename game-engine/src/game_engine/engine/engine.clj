(ns game-engine.engine.engine
  "Controls the flow of the game engine"
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]))

; (build-initial-state) -> map?
(defn build-initial-state
  "The game engine tracks the game state with a map.
  Returns a map representing the initial game state."
  []
  (-> {}
      (assoc :field-vector (vec (repeat  81 "0")))
      (assoc :macroboard-vector (vec (repeat 9 "-1")))
      (assoc :round 1)
      (assoc :move 1)
      (assoc :game-winner false)
      (assoc :moving-player :bot1)))

; (col-row->field-index [col-str row-str]) -> int?
; argument should be a seq with at least two members
; col-str -> str? representing a column # from 0 to 8
; row-str -> str? representing a row # from 0 to 8 
(defn col-row->field-index
  "Return the index in the field vector from 0 to 80 that
  matches the position in the 9 x 9 representation of the board"
  [[col-str row-str]]
  ; read-string converts a string to an int
  (+ (* 9 (read-string row-str)) (read-string col-str)))


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

;;; Validation Helpers
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

;;; Validations
(defn validate-input
  [input-str]
  ; starts with place_move
  ; split vector has length 3
  ; 2nd and 3rd values are int
  ; 2nd and 3rd values are between 0 and 8 inclusive
  (let [input-vec (clojure.string/split input-str #" ")
        validation-vec [#(= "place_move" (first %))
                        #(= 3 (count %))
                        #(every? int-within-range? (rest %))]]
    (every? true? (map #(% input-vec) validation-vec))))

; (validate-macroboard-placement col row macroboard-vector) -> bool?
; col -> int?
; row -> int?
; macroboard-vector -> vector? of int?
(defn validate-macroboard-placement
  "Return true if col and row are in an
  open macroboard"
  [col row macroboard-vector]
  ; macroboard is vector of strings
  (= "-1" (macroboard-vector (col-row->macroboard col row))))

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
      (loop [state (build-initial-state)
             bot-chan ((:moving-player state) bot-channels)]
        (if (< (:move state) 11)
          (do 
            ; update the bot with current state
            (>!! bot-chan (str "update game round " (:round state)))
            (>!! bot-chan (str "update game move " (:move state)))
            (>!! bot-chan (str "update game field " (clojure.string/join "," (:field-vector state))))
            (>!! bot-chan (str "update game macroboard " (clojure.string/join "," (:macroboard-vector state))))
            
            ; prompt the bot to move
            (>!! bot-chan "action move now")
            
            ; wait for and validate the response
            (let [bot-input (<!! my-chan)
                  move-vec (clojure.string/split bot-input #" ")
                  move-index (col-row->field-index (rest move-vec))
                  new-field (assoc (:field-vector state) move-index (bot-number (:moving-player state)))
                  new-macroboard ["0" "-1" "0" "0" "0" "0" "0" "0" "0"]
                  next-player (flip-player (:moving-player state))]
              (println (:moving-player state) "returns:" bot-input)
              (recur (assoc state :field-vector new-field :macroboard-vector new-macroboard :move (+ (:move state) 1) :moving-player next-player) (next-player bot-channels))))))))
