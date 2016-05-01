(ns game-engine.engine.engine
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close!]]))

(defn build-initial-state
  []
  (-> {}
      (assoc :field-vector (vec (repeat  81 "0")))
      (assoc :macroboard-vector (vec (repeat 9 "-1")))
      (assoc :round 1)
      (assoc :move 1)
      (assoc :game-winner false)
      (assoc :moving-player :bot1)))

(defn row-col->field-index
  [[col-str row-str]]
  (+ (* 9 (read-string row-str)) (read-string col-str)))

(defn flip-player
  [bot]
  (if (= bot :bot1)
    :bot2
    :bot1))

(defn bot-number
  [bot-id]
  (if (= bot-id :bot1)
    1
    2))

(defn run-game 
  [my-chan bot1-chan bot2-chan]
  
  ; inform bots of their id number
  (>!! bot1-chan "settings your_botid 1")
  (>!! bot2-chan "settings your_botid 2")
  
  (let [bot-channels {:bot1 bot1-chan :bot2 bot2-chan}]
      (loop [state (build-initial-state)

             bot-chan ((:moving-player state) bot-channels)]
        (if (< (:move state) 11)
          (do 
              (>!! bot-chan (str "update game round " (:round state)))
              (>!! bot-chan (str "update game move " (:move state)))
              (>!! bot-chan (str "update game field " (clojure.string/join "," (:field-vector state))))
              (>!! bot-chan (str "update game macroboard " (clojure.string/join "," (:macroboard-vector state))))
              (>!! bot-chan "action move now")
              (let [bot-input (<!! my-chan)
                    move-vec (clojure.string/split bot-input #" ")
                    move-index (row-col->field-index (rest move-vec))
                    new-field (assoc (:field-vector state) move-index (bot-number (:moving-player state)))
                    new-macroboard ["0" "-1" "0" "0" "0" "0" "0" "0" "0"]
                    next-player (flip-player (:moving-player state))]
                (println (:moving-player state) "returns:" bot-input)
                (recur (assoc state :field-vector new-field :macroboard-vector new-macroboard :move (+ (:move state) 1) :moving-player next-player) (next-player bot-channels))))))))
