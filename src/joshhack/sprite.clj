(ns joshhack.sprite
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Generation

(def sprites-map {})

(defn gen-sprites
  "add starting sprites"
  [world]
  (let [e (world/get-floor-tile world)]
    (assoc sprites-map :player e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Sprites Utils

(defn move-player
  [player w x y]
  (if (and (< 0 x) 
	   (< 0 y) 
	   (> (count w) x) 
	   (> (count (first w)) y)
	   (world/is-symbol? w y x :floor))
    [x y]
    player))