(ns basicbot.core
  (require [clojure.string :as str])
  (:gen-class))

(def input-routes
  {"settings" #(println "The type is: 'settings'")
   "move" #(println "The type is: 'move'")
   "action" #(println "The type is: 'action'")})

; (string-to-vector str rgx)
; str -> string?
; rgx -> regex expression to split string on
; returns vector of strings
(defn string-to-vector
  "Split a str on rgx"
  [str rgx]
  (str/split str rgx))

; (route-by-input-type v)
; v -> vector? of string?
(defn route-by-input-type
  "Call a function in input-routes based on the first item in v"
  [v]
  (let [type (v 0)]
    ((input-routes type))))

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
