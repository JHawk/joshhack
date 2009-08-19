(ns joshhack.player
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Player Generation

(defn- set-position
  "Adds an object, replaces an existing floor tile with type tile"
  [world player]
  (let [empty (world/get-floor-tile world)]
    (assoc player :position empty)))

(defn gen-player
  "Creates player"
  [world]
  (set-position world {}))

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