(ns basicbot.core
  (require [basicbot.board.board :as board]
           [basicbot.move.move :as move]
           [basicbot.io.io :as board-io])
  (:gen-class))

(defn -main
  ""
  [& args]
  (board-io/read-input))
