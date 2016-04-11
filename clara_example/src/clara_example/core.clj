(ns clara-example.core
  (:require [clara.rules :refer :all])
  (:gen-class))

(defrecord SupportRequest [client level])

(defrecord ClientRepresentative [name client])

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
