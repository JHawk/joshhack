(ns joshhack.npc
  (:require [joshhack.player :as player])
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Generation

(defstruct npc :position :tile)

(def tile-types [:bandit :snake])

(defn gen-npc
  "Creates npc"
  [world type]
  (struct npc 
	  (world/get-floor-tile world)
	  (nth tile-types type)))

(defn gen-random-npcs
  "Generates a vector of a random number of npcs"
  [world]
  (apply vector 
	 (take (+ 1 (rand-int 10)) 
	       (repeat (gen-npc world (rand-int (count tile-types)))))))

;(defn gen-random-npcs
;  [w]
;  (loop [npcs []
;	 n (+ 1 (rand-int 10))]
;    (if (zero? n)
;      npcs
;      (recur (conj npcs (struct npc (world/get-floor-tile w) (nth tile-types (- (count tile-types) 1)))) (dec n)))))

; gen-npc w (rand-int (count tile-types)))) (dec n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Utils

(defn move-non-player
  "Changes the players location if the new location is legal"
  [pos w]
  (let [dir (world/random-queen-dir)]
    (player/move-player pos w (first dir) (second dir))))