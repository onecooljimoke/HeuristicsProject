(ns clara-example.core
  (:require [clara.rules :refer :all])
  (:gen-class))

; what's the deal with these record things?
; http://www.braveclojure.com/multimethods-records-protocols/#Records
; a record is like declaring a new data type (under the hood it's
; actually a java class)
; records are really similar to a map, they have defined properties
; and even print to the screen like a map

(defrecord Parents [name mother father])
(defrecord Request [name])

(defrule print-ancestors
  ; rule will match any Request and Parents records on name, mother and father
  [Request (= ?name name)]
  [Parents (= ?name name) (= ?mother mother) (= ?father father)]
  =>
  (if ?mother
    (do
      (println ?mother "is an ancestor via" ?name)
      ; note that inside a rule we need to use insert! with an exclamation, ouside
      ; the rule use insert without an exclamation
      (insert! (->Request ?mother))))
  (if ?father
    (do
      (println ?father "is an ancestor via" ?name)
      (insert! (->Request ?father)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> (mk-session 'clara-example.core)
      (insert
       (->Request "penelope")
       (->Parents "penelope" "jessica" "jeremy")
       (->Parents "jessica" "mary-elizabeth" "homer")
       (->Parents "jeremy" "jenny" "steven")
       (->Parents "steven" "loree" "john")
       (->Parents "loree" false "jason")
       (->Parents "homer" "stephanie" false))
      (fire-rules))
  (println "End of program"))
