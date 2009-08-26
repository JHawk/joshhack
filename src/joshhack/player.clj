(ns joshhack.player
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Generation

(defstruct player :position :tile :attack :hit-points :vision)

;;; TODO - make sure player doesn't gen ontop of npc & col detection 
(defn gen-player
  "Creates player"
  [world]
  (struct player 
	  (world/get-floor-tile world)
	  :player
	  5
	  10
	  15))

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