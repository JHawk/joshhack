(ns joshhack.sprite
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Generation

(def sprites-map {})

(defn- add-sprite
  "Adds an object, replaces an existing floor tile with type tile"
  [world tile sprites]
  (let [empty (world/get-floor-tile world)]
    (assoc sprites tile empty)))

(defn gen-sprites
  "Adds starting sprites"
  [world]
  (add-sprite world :player 
	      (add-sprite world :stairs-up 
			  (add-sprite world :stairs-down sprites-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Utils

(defn move-player
  "Changes the players location if the new location is legal"
  [player w x y]
  (if (and (< 0 x) 
	   (< 0 y) 
	   (> (count w) x) 
	   (> (count (first w)) y)
	   (world/is-symbol? w y x :floor))
    [x y]
    player))