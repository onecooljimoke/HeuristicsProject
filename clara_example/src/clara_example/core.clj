(ns clara-example.core
  (:require [clara.rules :refer :all])
  (:gen-class))

; what's the deal with these record things?
; http://www.braveclojure.com/multimethods-records-protocols/#Records
; a record is like declaring a new data type (under the hood it's
; actually a java class)
; records are really similar to a map, they have defined properties
; and even print to the screen like a map
(defrecord SupportRequest [client level])

(defrecord ClientRepresentative [name client])

(defrule say-my-name
  [ClientRepresentative (= ?name name)]
  =>
  (println "Your name is: " ?name))

(defrule is-important
  "Find important support requests."
  [SupportRequest (= :high level)]
  =>
  (println "High support requested!"))

(defrule notify-client-rep
  "Find the client representative and request support."
  [SupportRequest (= ?client client)]
  [ClientRepresentative (= ?client client) (= ?name name)]
  =>
  (println "Notify" ?name "that"  
          ?client "has a new support request!"))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> (mk-session 'clara-example.core)
      (insert (->ClientRepresentative "Alice" "Acme")
              (->SupportRequest "Acme" :high))
      (fire-rules))
  (println "End of program"))
