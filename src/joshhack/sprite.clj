(ns joshhack.sprite
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Generation

(defn- add-sprite-rand
  "Adds an object, replaces an existing floor tile with type tile"
  ([world tile sprites]
     (assoc sprites tile (world/get-floor-tile world))))

(defn add-sprite-pos
  "Adds an object, at a pos"
  ([sprites tile pos]
    (assoc sprites tile pos)))

(defn gen-sprites
  "Adds starting sprites"
  [world]
  (add-sprite-rand world :stairs-down
	      (add-sprite-rand world :stairs-up {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Utils


