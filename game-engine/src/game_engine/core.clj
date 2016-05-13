(ns game-engine.core
  "Contains -main function for kicking off the program"
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]
            [game-engine.engine.engine :as engine]
            [game-engine.basicbot.bot :as basicbot]
            [game-engine.rulesbot.bot :as rulesbot])
  (:gen-class))

(defn -main
  [& args]
  ; make 3 channels for communicating between the game engine and the bots
  (dotimes [n 2]
    (let [engine-channel (chan)
          bot1-channel (chan)
          bot2-channel (chan)]
      ; kick off each bot on it's own thread
      ; the read input function will start a thread by itself
      (basicbot/read-input bot1-channel engine-channel)
      (rulesbot/read-input bot2-channel engine-channel)
      ; start the game engine running
      ; we can affect the player order by reversing the order of
      ; of bot1 and bot2 channel
      (engine/run-game engine-channel bot1-channel bot2-channel))))
