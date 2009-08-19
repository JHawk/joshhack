(ns joshhack.world)

(def min-floor-cover 0.25)
(def min-room 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Object Utils

(def count-if (comp count filter))

(defn is-symbol?
  "Return true if x y is a tile"
  [world y x tile]
  (= (nth (nth world x) y) tile))

(defn position-in-world? 
  "Checks if the x y is in bounds"
  [w x y]
  (and (< 0 x) 
       (< 0 y)
       (> (count w) x)
       (> (count (first w)) y)))

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

(defn world-size
  "Returns the area of the world"
  [world]
  (* (count (first world)) (count world)))

(defn total-tiles
  "Returns the total number of tiles in world of tile type"
  [world tile]
  (let [is-tile? (fn [x] (= x tile))]
    (count-if is-tile? (apply concat world))))

(defn tile-coverage
  "Divides the number of tile by total area and returns decimal"
  [world tile]
  (/ (total-tiles world tile) (world-size world)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World Generation

(defn- gen-empty-world 
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
       (apply vector (for [i (range 0 max-y)]
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

(defn- add-room
  "Adds a room to a world"
  [world]
  (let [max-x (count (first world))
	max-y (count world)
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

(def directions [[1 0] [-1 0] [0 1] [0 -1]
		 [1 1] [-1 1] [-1 -1] [1 -1]])

(defn draw-path
  [world start-x start-y steps dir tile]
  (loop [w world
	 x start-x
	 y start-y
	 s steps]
    (let [new-x (+ x (first dir))
	  new-y (+ y (second dir))]
      (if (or (zero? s) (not (position-in-world? w new-x new-y)))
	w
	(recur (assoc-in w [new-x new-y] tile) new-x new-y (dec s))))))

(defn random-tile-steps
  [world start-x start-y steps turns tile rand-dir]
  (loop [w world
	 x start-x
	 y start-y
	 s steps
	 t turns] 
    (if (zero? t)
      w
      (let [dir (rand-dir)
	    new-x (+ x (* steps (first dir)))
	    new-y (+ y (* steps (second dir)))
	    new-w (draw-path w x y s dir tile)]
	(recur new-w new-x new-y s (dec t))))))

(defn random-rook-tile-steps
  "Random change in position in the x or y direction takes a starting position and a magnitude" 
  [world start-x start-y steps turns tile]
  (let [rand-dir (fn [] (nth directions (rand-int 4)))]
    (random-tile-steps world start-x start-y steps turns tile rand-dir)))

(defn- add-pool
  "Adds a pool of water to a world"
  [world]
  (let [e (get-floor-tile world)]
    (random-rook-tile-steps world (first e) (second e) 1 (+ 10 (rand-int 40)) :water)))

;(defn- connect-room
;  "Connects rooms with tunnels if floor tiles do not overlap"
;  [world]
;  (if (only room or overlap)
;    world 
;    tunnel to floor tile not in this room))


 ;; add connect-room to recur when ready 
(defn- gen-world-with-rooms 
  "Generates a world with rooms covering min-floor-coverage of the map"  
  [max-x max-y]
  (loop [world (gen-empty-world max-x max-y)]
    (if (< min-floor-cover (tile-coverage world :floor))
      world
      (recur (add-room world)))))

(defn gen-world
  "Makes a world with random rooms and adds starting objects"
  [max-x max-y]
  (add-pool 
   (gen-world-with-rooms max-x max-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Draw Utils

(def symbol-world
     {

      ;;; world 
      :none "."
      :floor " "
      :wall "#"
      :water "~"

      ;;; sprites
      :stairs-down ">"
      :stairs-up "<"
      :player "@"})

(defn- render-sprites
  "Draws sprites over the world map"
  [world sprites]
  (loop [w world
	 s sprites]
    (if (empty? s)
      w
      (let [tile (first (first s))
	    x (first (second (first s)))
	    y (second (second (first s)))]
	(recur (assoc-in w [x y] tile) (rest s))))))

(defn draw-world 
  "Returns a string representation of the world with sprites"
  [world sprites]
  (apply str (for [row (render-sprites world sprites)] 
	       (apply str (concat (for [token (doall row)] 
				  (symbol-world token))
				[\newline])))))
