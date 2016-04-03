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

; (string-to-vector str rgx) -> vector?
; str -> string?
; rgx -> regex expression to split string on
; returns vector of strings
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

; (big-squares-available str) -> list?
; str -> string?
(defn big-squares-available
  "Return a list of macorboard squares that a move can be made in"
  [str]
  ; map-indexed gives us the index and each item in a vector
  (let [lst (map-indexed available-for-move? (string-to-vector str #","))]
    ; remove false values from the list, leaving us with macroboard numbers
    (filter #(not (= false %)) lst)))

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

(defn -main
  ""
  [& args]
  (read-input))
