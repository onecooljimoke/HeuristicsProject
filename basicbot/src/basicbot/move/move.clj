(ns basicbot.move.move)

; (pick-move)
; mb-available-cells -> int?
(defn pick-move
  "returns a valid index in the macroboard list if
  the list is non-empty, otherwise it returns -1"
  [mb-available-cells]
  (if (empty? mb-available-cells)
    -1
    (rand-nth mb-available-cells)))
