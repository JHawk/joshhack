(ns joshhack.npc
  (:require [joshhack.player :as player])
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Generation

(defstruct npc :position :tile :attack :hit-points :dead)

(def tile-types [:bandit :snake :zombie :squirrel])

(defn gen-npc
  "Creates npc"
  [world type]
  (struct npc 
	  (world/get-floor-tile world)
	  (nth tile-types type)
	  5
	  10
	  false))

(defn gen-random-npcs
  [w]
  (loop [npcs []
	 n (+ 1 (rand-int (quot (* (world/world-size w) 0.008) 1)))]
    (if (zero? n)
      npcs
      (recur (conj npcs (gen-npc w (rand-int (count tile-types)))) (dec n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Utils

;;; TODO - make this more interesting 
(defn take-damage
  "Takes a number of hit-points and a number to dec and returns new number of hit-points"
  [hp attack]
  (- hp attack))

(defn move-non-player
  "Changes the npcs location if the new location is legal - calls into player"
  [pos w]
  (let [dir (world/random-queen-dir)]
    (player/move-player pos w (+ (first pos) (first dir)) (+ (second pos) (second dir)))))

