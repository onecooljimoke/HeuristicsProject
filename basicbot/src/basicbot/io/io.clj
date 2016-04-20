(ns basicbot.io.io
  "Functions for input and output"
  (:require [basicbot.board.board :as board]
            [basicbot.move.move :as move]))

; input-routes -> map?
(def input-routes
  "A map for referencing functions to call based on the
  first word of the game input string. The keys are the
  potential first words, the values are the functions to
  call."
  {"settings" board/game-input-starts-with-settings 
   "update" board/game-input-starts-with-update 
   "action" move/game-input-starts-with-action})


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
      (println "Error: can't find: " type))))

;(read-input)
; listen to standard-input and write it to standard output
(defn read-input
  "Use java.io.BufferedReader and .BufferedWriter to read
  continuous user input

  Stop listening by typing 'end' "
  []
  (println "Now listening for input")
  (let [rdr (java.io.BufferedReader. *in*)
        wrt (java.io.BufferedWriter. *out*)]
    (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
      (let [output (route-by-input-type (board/string->vector ln #" "))]
       (if output
         (do (.write wrt ln)
             ; .write doesn't print newlines
             (.newLine wrt)
             (.write wrt output)
             (.newLine wrt)
             ; flush the buffer to output
             (.flush wrt)))))
    ; close rdr because we're considerate programmers
    ; closing wrt seems to cause the program to crash
    (.close rdr)))
