(ns io-operations.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn read-line-clojure-funcs
  "Use println, readln and recur to read
  continuous user input

  Stop by typing 'end' "
  []
  ; from looking at source code it seems that 
  ; a new instance of java.io.BufferedReader/BufferedWriter
  ; is created on each call to println and read-line 
  (println "** read-line-clojure-funcs**")
  (loop [ln (read-line)]
    (if (= ln "end")
      (println "Bye Bye")
      (do
        (println ln)
        (recur (read-line))))))

(defn read-line-clojure-io
  "Use clojure.io.reader and line-seq to read
  continuous user input

  Stop by typing 'end "
  []
  ; use clojure-io for convenience in creating buffered reader and writer
  (println "** read-line-clojure-io **")
  (let [wrt (io/writer *out*)]
    ; with-open is a macro that insures stream class is closed
    ; upon completion
    (with-open [rdr (io/reader *in*)]
      (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
        (.write wrt ln)
        (.newLine wrt)
        (.flush wrt))) ; end of with-open
    ; clean up writer
    ; note that we don't call .close on writer as it caused errors
    ; in the repl and when using lein run
    (.write wrt "Goodbye")
    (.newLine wrt)
    (.flush wrt)))

(defn read-line-java-io
  "Use java.io.BufferedReader and println to read
  continuous user input.

  Stop by typing 'end' "
  []
  (println "** read-line-java-io **")
  (let [rdr (java.io.BufferedReader. *in*)]
    (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
      ; println creates new BufferedWriter each time
      (println ln))
    (.close rdr)
    (println "Goodbye")))

; !!! use this style for input/output
(defn read-line-java-io-2
  "Use java.io.BufferedReader and .BufferedWriter to read
  continuous user input

  Stop by typing 'end' "
  []
  (println "** read-line-java-io-2 **")
  (let [rdr (java.io.BufferedReader. *in*)
        wrt (java.io.BufferedWriter. *out*)]
    (doseq [ln (take-while #(not (= "end" %)) (line-seq rdr))]
      (.write wrt ln)
      (.newLine wrt)
      (.flush wrt))
    (.close rdr)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Enter some text, or 'end' when done")
  (read-line-java-io-2))


  
