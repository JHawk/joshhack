(ns joshhack.world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Object Utils

(defn is-symbol?
  "pred - if tile is floor"
  [world y x s]
  (= (nth (nth world x) y) s))

(defn get-floor-tile
  "find an empty floor tile"
  [world]
  (let [guess-x (rand-int (- (count (first world)) 1))
	guess-y (rand-int (- (count world) 1))]
    (if (is-symbol? world guess-x guess-y :floor)
      (vector guess-y guess-x)
      (get-floor-tile world))))

(defn- add-object
  "adds an object, replaces an existing floor tile"
  [world symbol]
  (let [empty (get-floor-tile world)]
    (assoc-in world empty symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World Generation

(defn gen-empty-world 
  "Generates an empty world"
  [max-x max-y] 
  (apply vector (take max-y (repeat (apply vector (take max-x (repeat :none)))))))

(defn- draw-room
  "Draws a room at the supplied position, size, etc."
  ([world x y width height texture]
     (draw-room world x y width height texture nil))
  ([world x y width height texture noop]
     (let [max-x (count (first world))
	   max-y (count world)]
       (apply vector(for [i (range 0 max-y)]
		      (if (and (> i y)
			       (<= i (+ y height)))
			(apply vector (for [j (range 0 max-x)]
					(if (and (> j x)
						 (<= j (+ x width))
						 (or (nil? noop)
						     (not (= (nth (nth world i) j) noop))))
					  texture
					  (nth (nth world i) j))))
			(nth world i)))))))

(defn add-room
  "Adds a room to a world"
  [world]
  (let [max-x (count (first world))
	max-y (count world)
	min-room 5
	width (+ min-room (rand-int 4))
	height (+ min-room (rand-int 4))
	x (rand-int (- max-x width 1))
	y (rand-int (- max-y height 1))]
    (draw-room (draw-room world x y width height :wall :floor)
	       (+ x 1)
	       (+ y 1)
	       (- width 2)
	       (- height 2)
	       :floor)))

(defn gen-world-with-rooms 
  [x max-x max-y]
  "gens a world with x rooms"
  (loop [world (gen-empty-world max-x max-y)
	 c x]
    (if (= c 0)
      world
      (recur (add-room world) (- c 1)))))

(defn gen-world
  "makes a world and binds it to ref game-world"
  [max-x max-y]
  (add-object 
   (gen-world-with-rooms (+ 1 (rand-int 4) max-x max-y)) 
   :stairs-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Draw Utils

(def symbol-world
     {:none "."
      :floor " "
      :wall "#"
      :stairs-down ">"
      :stairs-up "<"
      :player "@"})

(defn render-sprites
  "places the sprites on the world map"
  [world sprites]
  (loop [w world
	 s sprites]
    (if (empty? s)
      w
      (let [tile (first (first s))
	    x (first (second (first s)))
	    y (second (second (first s)))]
	(recur (assoc-in world [x y] tile) (rest s))))))

(defn draw-world 
  [world sprites]
  (apply str (for [row (render-sprites world sprites)] 
	       (apply str (concat (for [token (doall row)] 
				  (symbol-world token))
				[\newline])))))
