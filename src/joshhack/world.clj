(ns joshhack.world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Object Utils

(defn is-symbol?
  "Return true if x y is a tile"
  [world y x tile]
  (= (nth (nth world x) y) tile))

(defn get-floor-tile
  "Find a random floor tile"
  [world]
  (let [guess-x (rand-int (- (count (first world)) 1))
	guess-y (rand-int (- (count world) 1))]
    (if (is-symbol? world guess-x guess-y :floor)
      (vector guess-y guess-x)
      (get-floor-tile world))))

(defn- add-object
  "Adds an object, replaces an existing floor tile with type tile"
  [world tile]
  (let [empty (get-floor-tile world)]
    (assoc-in world empty tile)))

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
  "Generates a world with x rooms"  
  [x max-x max-y]
  (loop [world (gen-empty-world max-x max-y)
	 c x]
    (if (= c 0)
      world
      (recur (add-room world) (- c 1)))))

(defn gen-world
  "Makes a world with random rooms and adds starting objects"
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
  "Draws sprites over the world map"
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
  "Returns a string representation of the world with sprites"
  [world sprites]
  (apply str (for [row (render-sprites world sprites)] 
	       (apply str (concat (for [token (doall row)] 
				  (symbol-world token))
				[\newline])))))
