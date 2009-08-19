(ns joshhack.sprite
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Generation

(defn- add-sprite
  "Adds an object, replaces an existing floor tile with type tile"
  [world tile sprites]
  (let [empty (world/get-floor-tile world)]
    (assoc sprites tile empty)))

(defn gen-sprites
  "Adds starting sprites"
  [world]
  (add-sprite world :stairs-down
	      (add-sprite world :stairs-up 
			  (add-sprite world :monster {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Utils


