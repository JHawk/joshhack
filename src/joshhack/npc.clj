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
  [w]
  (loop [npcs []
	 n (+ 1 (rand-int 10))]
    (if (zero? n)
      npcs
      (recur (conj npcs (struct npc (world/get-floor-tile w) (nth tile-types (- (count tile-types) 1)))) (dec n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Utils

(defn move-non-player
  "Changes the players location if the new location is legal"
  [pos w]
  (let [dir (world/random-queen-dir)]
    (player/move-player pos w (first dir) (second dir))))