(ns joshhack.npc
  (:require [joshhack.player :as player])
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Generation

(defstruct npc :position :tile :attack :hit-points :vision :dead)

(def tile-types [:bandit :snake :zombie :squirrel])

(defn gen-npc
  "Creates npc"
  [world type]
  (struct npc 
	  (world/get-floor-tile world)
	  (nth tile-types type)
	  5
	  10
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

(defn attacked?
  [{pos :position dead :dead :as npc} pos-attacked] 
  (and (= pos pos-attacked) (not dead)))

(defn some-npc-defending?
  [pos-attacked npcs]
  (some (fn [npc] (attacked? npc pos-attacked)) npcs))

(defn receive-attack
  [attack pos-attacked npcs]
  (for [{hp :hit-points :as npc} npcs] 
    (if (attacked? npc pos-attacked)
      (assoc npc :hit-points (- hp attack))
      npc)))

(defn make-dead 
  [npc] 
  (assoc npc :dead true :tile :dead-body))

(defn move-non-player
  "Changes the npcs location if the new location is legal - calls into player"
  [pos w]
  (player/get-new-pos pos w (world/random-queen-dir)))

(defn do-npc-turns
  [npcs w]  
  (for [npc npcs] 
    (if (>= 0 (:hit-points npc))
      (make-dead npc)
      (assoc npc :position 
	     (move-non-player 
	      (npc :position) 
	      w)))))



