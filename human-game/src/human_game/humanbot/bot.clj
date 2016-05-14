(ns human-game.humanbot.bot
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go go-loop chan buffer close!]]))

(defn write-field-vector
  [field-string]
  (println "Field Vector:")
  (println "   0 1 2 3 4 5 6 7 8")
  (println "   -----------------")
  (let [field-vector (clojure.string/split field-string #",")]
    (doall (map #(println (str %2 "|") (clojure.string/join #" " %1)) (partition 9 field-vector) (range 9)))))

(defn write-macroboard-vector
  [macro-string]
  (println "Macroboard:")
  (let [macro-vector (clojure.string/split macro-string #",")]
    (doall (map #(println (clojure.string/join #" " %)) (partition 3 macro-vector)))))

; (game-input-starts-with-settings v) -> nil
; v -> vector? of string?
(defn game-input-starts-with-settings 
  "Input from the game server that starts with 'settings' is
  intended to update static information about the game such
  as bot ids."
  [v]
  (println (clojure.string/join #" " (rest v))))

; (game-input-starts-with-update v) -> nil
; v -> vector? of string?
(defn game-input-starts-with-update
  "Input from the game server that starts with update is
  intended to update information that changes periodically
  throughout the game such as the field list and the macroboard
  list."
  [v]
  ; settings input will look like:
  ; "update game field <str>"
  ; "update game macroboard <str>
  (cond
    (and (= (v 1) "game")
         (= (v 2) "field"))
    (write-field-vector (v 3))
    (and (= (v 1) "game")
         (= (v 2) "macroboard"))
    (write-macroboard-vector (v 3))
    :else (println (clojure.string/join #" " (rest v)))))

; input-routes -> map?
(def input-routes
  "A map for referencing functions to call based on the
  first word of the game input string. The keys are the
  potential first words, the values are the functions to
  call."
  {"settings" game-input-starts-with-settings 
   "update" game-input-starts-with-update})

; (route-by-input-type v) -> nil?
; v -> vector? of string?
(defn route-by-input-type
  "Given a vector of strings, v, call a function in input-routes based
  on the first string in v"
  [v]
  (let [type (v 0)]
    ; make sure v is a key in input-routes
    (if (contains? input-routes type)
      ((input-routes type) v)
      (println "Can't find route: " v))))

(defn format-move
  [move-str]
  (str "place_move " move-str))

(defn read-human-input
  [in-chan out-chan]
  (go-loop [ln (clojure.string/trim (<! in-chan))]
    (let [input-vec (clojure.string/split ln #" ")]
      (if (not (= "action" (input-vec 0)))
        (route-by-input-type input-vec)
        (do 
          (println "Enter move as 'col row':")
          (>! out-chan (format-move (read-line)))))
      (recur (clojure.string/trim (<! in-chan))))))
