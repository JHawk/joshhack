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
  
(defn add-stairs
  [w {pl :previous-level cl :current-level pos :position :as player} sprites]
  (if (< pl cl)
    (if (= cl 0)
      (add-sprite-rand w :stairs-down sprites)
      (add-sprite-pos (add-sprite-rand w :stairs-down sprites) :stairs-up pos))
    (add-sprite-pos (add-sprite-rand w :stairs-up sprites) :stairs-down pos)))

(defn add-stairs-down
  [w p]
  ())

(defn gen-sprites
  "Adds starting sprites"
  [w p]
  (add-stairs w p
	      (add-sprite-rand w :equipment {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Utils
