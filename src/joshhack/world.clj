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
       (> (- (count w) 1) x)
       (> (- (count (first w)) 1) y)))

(defn max-steps 
  "Finds the max number of steps in the x or y direction that are in bounds"
  [world dir pos]
  (max (condp = dir
	 [1 0] (- (count world) (first pos) 3)
	 [0 1] (- (count (first world)) (second pos) 3)
	 [-1 0] (- (first pos) 3)
	 [0 -1] (- (second pos) 3))
       0))

(defn end-point
  [start-x start-y mag dir]
  [(+ start-x (* (first dir) mag)) (+ start-y (* (second dir) mag))])

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

(defn random-rook-dir [] (nth directions (rand-int 4)))

(defn random-queen-dir [] (nth directions (rand-int 8)))

(defn draw-path
  ([world start-x start-y steps dir tile]
     (draw-path world start-x start-y steps dir tile nil))
  ([world start-x start-y steps dir tile noop]
     (loop [w world
	    x start-x
	    y start-y
	    s steps]
       (let [new-x (+ x (first dir))
	     new-y (+ y (second dir))]
	 (if (or (<= s 0) (not (position-in-world? w new-x new-y)))
	   w
	   (let [this-tile (if (and (not (nil? noop)) 
				    (is-symbol? w new-y new-x noop)) 
			     noop
			     tile)]
	     (recur (assoc-in w [new-x new-y] this-tile) new-x new-y (dec s))))))))

(defn try-tunnel
  [w x y steps dir] 
  (let [end-point (end-point x y steps dir)]
    (if (is-symbol? w (second end-point) (first end-point) :floor)
      (draw-path 
       (draw-path 
	(draw-path w x y steps dir :floor) 
	(- x (second dir)) (- y (first dir)) steps dir :wall :floor) 
       (+ x (second dir)) (+ y (first dir)) steps dir :wall :floor)
      w)))

;;; TODO - make it not double back
(defn- random-tile-steps
  "Changes tiles from starting position takes number of steps, number of turns, a tile, and a means of finding a direction" 
  ([world start-x start-y steps turns tile rand-dir]
     (random-tile-steps world start-x start-y steps turns tile rand-dir nil))  
  ([world start-x start-y steps turns tile rand-dir noop]
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
	       new-w (draw-path w x y s dir tile noop)]
	   (recur new-w new-x new-y s (dec t)))))))

(defn random-rook-tile-steps
  "Wraps random-tile-steps only allowing turns or changes in direction along the x and y axis" 
  ([world start-x start-y steps turns tile]
     (random-rook-tile-steps world start-x start-y steps turns tile nil))
  ([world start-x start-y steps turns tile noop]
     (let [rand-dir random-rook-dir] ;;; random-rook-dir may need to be in ()
       (random-tile-steps world start-x start-y steps turns tile rand-dir noop))))

(defn random-queen-tile-steps
  "Wraps random-tile-steps allowing turns or changes in any direction" 
  ([world start-x start-y steps turns tile]
     (random-queen-tile-steps world start-x start-y steps turns tile nil))
  ([world start-x start-y steps turns tile noop]
     (let [rand-dir random-queen-dir]
       (random-tile-steps world start-x start-y steps turns tile rand-dir noop))))

(defn- add-pool
  "Adds a pool of water to a world"
  [world]
  (let [e (get-floor-tile world)]
    (random-queen-tile-steps world 
			     (first e) 
			     (second e) 
			     1 
			     (+ 10 (rand-int 100)) :water)))

;;; make this work 
(defn- add-bare-cave-walls
  [world]
  (let [e (get-floor-tile world)]
    (random-queen-tile-steps world (first e) (second e) 1 (+ 5 (rand-int 40)) :none :floor)))

(defn- add-random-passage
  [world]
  (let [e (get-floor-tile world)]
    (random-rook-tile-steps world (first e) (second e) (+ 1 (rand-int 8)) (rand-int 4) :floor :water)))

(defn- direct-or-random-tunnel
  [w e]
  (if (< 1 (rand-int 50))
    (add-random-passage w)
    (let [dir (random-rook-dir)]
      (try-tunnel w 
		  (first e) 
		  (second e) 
		  (rand-int (max-steps w dir e)) dir))))

(defn- connect-room
  "Connects rooms with tunnels if floor tiles do not overlap"
  [world connection-attempts]
  (loop [w world
	 t connection-attempts]
    (if (zero? t)
      w
      (let [e (get-floor-tile w)
	    dir (random-rook-dir)] 
	(recur (try-tunnel w 
			   (first e) 
			   (second e) 
			   (rand-int (max-steps w dir e)) dir) 
	       (dec t))))))

(defn- gen-world-with-rooms 
  "Generates a world with rooms covering min-floor-coverage of the map"  
  [max-x max-y]
  (loop [world (gen-empty-world max-x max-y)
	 connection-attempts 0]
    (if (< min-floor-cover (tile-coverage world :floor))
      world
      (recur (connect-room (add-room world) connection-attempts) 
	     (inc connection-attempts)))))

(def destruct-world
     [add-bare-cave-walls add-pool add-random-passage])

(defn gen-world
  "Makes a world with random rooms and adds starting objects"
  [max-x max-y]
  (let [world (gen-world-with-rooms max-x max-y)
	x (+ 2 (rand-int (quot (max max-x max-y) 10)))]
    (loop [w world
	   t x]
      (if (zero? t)
	w
	(recur ((nth destruct-world (rand-int (count destruct-world))) w) (dec t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Draw Utils

(def symbol-world
     {

      ;;; world 
      :floor " "
      :none ":"
      :wall "#"
      :water "~"

      ;;; sprites
      :stairs-down ">"
      :stairs-up "<"
      
      ;;; npcs
      :bandit "B"
      :snake "s"
      :zombie "Z"
      :squirrel "?"

      :dead "-"
      
      ;;; player
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

(defn- render-npcs
  [world npcs vision x y]
  (let [los (for [v (range (- x vision) (+ x vision))]
	      (for [h (range (- y vision) (+ y vision))]
		[v h]))]
    (loop [w world
	   n npcs]
      (if (empty? n)
	w
	(let [new-w (if (filter (= (:position n)) los)
		      (assoc-in w (:position (first n)) (:tile (first n)))
		      w)]
	(recur new-w (rest n)))))))

(defn- render-player
  "Draws sprites over the world map"
  [w player]
  (assoc-in w (:position player) (:tile player)))


;; TODO This draws the world at a 90 degree angle!!!
(defn draw-world
  "Returns a string representation of the world with sprites"
  [world sprites {[x y] :position vision :vision :as player} npcs]
  (let [size (int (* vision 1.5))
	w (render-player
	   (render-npcs
	    (render-sprites world sprites)
	    npcs vision x y)
	   player)]
    (apply str
	   (for [row (range (- x size) (+ x size))]
	     (apply str
		    (concat (for [token (range (- y size) (+ y size))]
			      (if (or (< row 0)
				      (< token 0)
				      (> row (- (count w) 1))
				      (> token (- (count (first w)) 1)))
				(symbol-world :none)
				(symbol-world ((w row) token))))
			    [\newline]))))))