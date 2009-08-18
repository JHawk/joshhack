(ns joshhack.world)

(def min-floor-cover 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Object Utils

(def count-if (comp count filter))

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
  (add-object 
   (gen-world-with-rooms max-x max-y) 
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


;;;;;;;;;;;;;;;;;;;; testing 

(defn print-world 
  [world]
  (doseq [token (doall world)] 
    (print (symbol-world token)))
  (println))

(comment 
  (print-world (gen-world 30 30))
)
