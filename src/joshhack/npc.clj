(ns joshhack.npc
  (:require [joshhack.player :as player])
  (:require [joshhack.world :as world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Generation

(defstruct npc :attack :hit-points :vision :position :tile :mood :destination :last-action :dead)

(def tile-types [:bandit :snake :zombie :squirrel])

(def mood-types [:hostile :neutral :friendly :asleep])

(defn gen-npc
  "Creates npc"
  [world type mood]
  (struct npc
	  
	  ;;; stats
	  2 ; attack
	  10; hit points 
	  10; vision

	  ;;; state
	  (world/get-floor-tile world) ; position 
	  (nth tile-types type) ; tile
	  (nth mood-types mood) ; mood 
	  :none ; destination
	  :none ; last action 
	  false ; dead flag
	  ))

(defn gen-random-npcs
  [w]
  (loop [npcs []
	 n (+ 1 (rand-int (quot (* (world/world-size w) 0.008) 1)))]
    (if (zero? n)
      npcs
      (recur (conj npcs (gen-npc w (rand-int (count tile-types)) (rand-int (count mood-types)))) (dec n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Non-Player Utils

; check tile to see 
; if it is of type :floor :water
; + direction map such as compass to that tile and get a seq of vectors 
; if the new vectors are of :floor :water 
; - add the same compass key to them until los exhausted or tile excludes 

(defn melee-range
  [{pos :position :as npc} dirs]
  (for [dir dirs] 
    [(+ (first dir) (first pos)) (+ (second dir) (second pos))]))

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

(defn move
  [dir {pos :position la :last-action :as npc} w p]
  (let [new-pos (player/get-new-pos pos w dir)]
    (if (and (not (= (:postion p) new-pos)) (not (= la :attacked)))
      (assoc npc :position new-pos :last-action :moved)
      (assoc npc :last-action :none))))

(defn move-npcs
  [npcs w p]  
  (for [npc npcs] 
    (if (>= 0 (:hit-points npc))
      (make-dead npc)
      (let [dir (world/random-queen-dir)]
	(move dir npc w p)))))



