(ns joshhack
  (:gen-class)
  (:import [javax.swing JFrame JTextArea JPanel]
	   [java.awt Font]
	   [java.awt.event KeyListener KeyEvent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World State

(def max-x 20)
(def max-y 20)

(def world-state (ref :none)
(def sprites-state (ref :none))

(defn alter-game-state
  "takes functions executes each and set-refs world-state and sprites-state"
  [f]
  ;; TODO 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Object Utils

(defn is-symbol?
  "pred - if tile is floor"
  [y x s]
  (= (nth (nth @world-state x) y) s))

(defn- get-floor-tile
  "find an empty floor tile"
  []
  (let [guess-x (rand-int (- (count (first @world-state)) 1))
	guess-y (rand-int (- (count @world-state) 1))]
    (if (is-symbol? guess-x guess-y :floor)
      (vector guess-y guess-x)
      (get-floor-tile))))

(defn- add-object
  "adds an object, replaces an existing floor tile"
  [symbol]
  (let [empty (get-floor-tile)]
    (dosync (alter world-state assoc-in empty symbol))))


(defn- add-sprite
  "adds sprite, sets position"
  [symbol]
  (let [empty (get-floor-tile)] 
    (dosync (alter sprites-state assoc symbol empty))))

(defn move-player
  [v h]
  (let [y (+ (first player) v)
	x (+ (second player) h)]
  (if (and (< 0 x) 
	   (< 0 y) 
	   (> (count (first @world-state)) x)
	   (> (count @world-state) y) 
	   (is-symbol? x y :floor))
    (dosync (alter sprites-state assoc :player [x y])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World Utils


;;;;;;;;;;;;;;;;;;;; made it to here 


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World Generation

(defn gen-empty-world 
  "Generates an empty world"
  [] 
  (apply vector (take max-y (repeat (apply vector (take max-x (repeat :none)))))))

(defn gen-world-with-rooms [x]
  "gens a world with x rooms"
  (loop [world (gen-empty-world)
	 c x]
    (if (= c 0)
      world
      (recur (add-room world) (- c 1)))))

(defn gen-world
  "makes a world and binds it to ref game-world"
  []
  (add-object 
   (gen-world-with-rooms (+ 1 (rand-int 4))) 
   :stairs-down))

(defn gen-sprites
  "add starting sprites"
  [world]
  (let [e (get-floor-tile world)
	sprites-map {}]
    (assoc sprites-map :player e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; Draw Functions

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

; replaced with draw-world
(defn print-world 
  [world sprites]
  (doseq [row (render-sprites world sprites)] 
    (doseq [token (doall row)] 
      (print (symbol-world token)))
    (println)))

(defn draw-world 
  [world sprites]
  (apply str (for [row (render-sprites world sprites)] 
	       (apply str (concat (for [token (doall row)] 
				    (symbol-world token))
				  [\newline])))))

(defn draw-init-world
  []
  (let [w gen-world]
    (draw-world w (gen-sprites w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World State

;(def world-state (ref (gen-world)))

;(def sprites-state (ref (gen-sprites @world-state)))

;(defn alter-game-state
;  "takes functions executes each and set-refs world-state and sprites-state"
;  [f])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Listener / JFrame / Main 

(def keyboard-handler
     (proxy [KeyListener] []
       (keyTyped [ke]
		 (let [c (. ke getKeyChar)]
		   (condp = c
		     "a" (move-player -1 0)
		     "q" (move-player -1 -1)
		     "w" (move-player  0 -1)
		     "e" (move-player 1 -1)
		     "d" (move-player 1 0)
		     "c" (move-player 1 1)
		     "x" (move-player 0 1)
		     "z" (move-player -1 1))))
       (keyPressed [ke] nil)
       (keyReleased [ke] nil)))

(defn -main
  [& args]
  (doto (JFrame.)
    (.setSize (* 8 max-x) (+ 25 (* 18 max-y)))
    (.add (doto (JPanel.)
	    (.add (doto (JTextArea. max-x max-y)
		    (.setEditable false)
		    (.setFont (Font. "Monospaced" (. Font PLAIN) 14 ))
		    (.setText (draw-world @world-state @sprites-state))
		    (.addKeyListener keyboard-handler)))))
    (.setResizable false)
    (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
    (.setVisible true)))

;;;;;; testing 

(comment
  (do (let [w (gen-world)]
	(print-world w (gen-sprites w)) 
	(println)))
)
