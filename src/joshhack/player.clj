(ns joshhack.player
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Generation

(defstruct player :attack :hit-points :vision :position :tile :current-level :previous-level)

;;; TODO - make sure player doesn't gen ontop of npc & col detection 
(defn gen-player
  "Creates player"
  [world]
  (struct player 

	  ;;; stats
	  5 ; attack
	  10; hit points 
	  10; vision

	  ;;; state
	  (world/get-floor-tile world)
	  :player
	  0
	  -1
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Utils

(defn- occupiable?
  "Checks if the new location is legal"
  [w x y]
  (and (world/position-in-world? w x y)
       (or (world/is-symbol? w x y :floor) 
	   (world/is-symbol? w x y :water))))

(defn get-new-pos
  [pos w dir]
  (let [new-x (+ (first pos) (first dir))
	new-y (+ (second pos) (second dir))]
    (if (occupiable? w new-x new-y)
      [new-x new-y]
      pos)))