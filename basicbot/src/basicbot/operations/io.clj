(ns basicbot.operations.io)

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
  "Return a string in the correct format for output to the game.
  Expects a list whose first item is the column number and whose
  second item is a row number"
  [move-lst]
  (str "place_move " (first move-lst) " " (second move-lst)))

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
      (.write wrt ln)
      ; .write doesn't print newlines
      (.newLine wrt)
      ; flush the buffer to output
      (.flush wrt))
    ; close rdr because we're considerate programmers
    ; closing wrt causes the program to crash
    (.close rdr)))
