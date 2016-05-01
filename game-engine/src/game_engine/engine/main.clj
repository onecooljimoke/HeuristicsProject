(ns game-engine.engine.main
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]))

(defn run-game 
  [my-chan bot1-chan bot2-chan]
  
  ; try calling bot 1
  (println "start passing input to bot1")
  (>!! bot1-chan "settings your_botid 1")
  (>!! bot1-chan "update game round 1")
  (>!! bot1-chan "update game move 1")
  (>!! bot1-chan "update game field 1,1,2,1,0,0,0,1,0,0,2,2,0,0,0,1,0,0,0,0,1,1,0,1,2,0,0,0,0,0,0,0,0,0,1,2,2,2,0,2,0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,2,2,0,1,2,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0")
  (>!! bot1-chan "update game macroboard -1,0,0,0,0,0,0,0,0")
  (>!! bot1-chan "action move now")
  (println "bot 1's move:" (<!! my-chan))
  
  ; try calling bot2
  (println "start passing input to bot2")
  (>!! bot2-chan "settings your_botid 1")
  (>!! bot2-chan "update game round 1")
  (>!! bot2-chan "update game move 1")
  (>!! bot2-chan "update game field 1,1,2,1,0,0,0,1,0,0,2,2,0,0,0,1,0,0,0,0,1,1,0,1,2,0,0,0,0,0,0,0,0,0,1,2,2,2,0,2,0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,2,2,0,1,2,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0")
  (>!! bot2-chan "update game macroboard -1,0,0,0,0,0,0,0,0")
  (>!! bot2-chan "action move now")
  (println "bot 2's move:" (<!! my-chan)))
