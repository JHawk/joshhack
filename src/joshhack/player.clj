(ns joshhack.player
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Generation

(defstruct player :position)

;;; TODO - make sure player doesn't gen ontop of npc 
(defn gen-player
  "Creates player"
  [world]
  (struct player 
	  (world/get-floor-tile world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Utils

(defn move-player
  "Changes the players location if the new location is legal"
  [pos w x y]
  (if (and (world/position-in-world? w x y)
	   (or (world/is-symbol? w y x :floor) (world/is-symbol? w y x :water)))
    [x y]
    pos))