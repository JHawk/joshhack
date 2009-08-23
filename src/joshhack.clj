(ns joshhack
  (:gen-class)
  (:import [javax.swing JFrame JTextArea JPanel]
	   [java.awt Font]
	   [java.awt.event KeyListener KeyEvent])
  (:require [joshhack.world :as world]
	    [joshhack.sprite :as sprite]
	    [joshhack.player :as player]
	    [joshhack.npc :as npc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World State

(def max-x 30)
(def max-y 30)

(def world-state (ref (world/gen-world max-x max-y)))
(def sprite-state (ref (sprite/gen-sprites @world-state)))
(def npc-state (ref (npc/gen-random-npcs @world-state)))
(def player-state (ref (player/gen-player @world-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Game Loop 

(def compass {:north [0, -1]
	      :south [0, 1]
	      :east [1, 0]
	      :west [-1, 0]
	      :northeast [1, -1]
	      :northwest [-1, -1]
	      :southeast [1, 1]
	      :southwest [-1, 1]})

;; TODO most of this logic needs to go in player - the things that should remain here are the 'glue'
;; that ties together calls to npc and player
(defn move 
  [dir]
  (let [position (@player-state :position)
	x (+ (first position) (first (compass dir)))
	y (+ (second position) (second (compass dir)))
	new-pos (player/move-player 
		 position
		 @world-state
		 x y)]
    (if (= @npc-state 
	   (ref-set npc-state 
		    (for [npc @npc-state]
		      (assoc npc :hit-points 
			     (if (and (= (npc :position) new-pos) (not (npc :dead)))
			       (npc/take-damage 
				(npc :hit-points) 
				(@player-state :attack))
			       (:hit-points npc))))))
      (alter player-state assoc :position new-pos))))

;; TODO refactor this down to just the game-loop steps
(defn- do-turn 
  [dir]
  (dosync (let [make-dead (fn [] ;; TODO refactor me to npcs
			    (do (ref-set npc-state
					 (for [npc @npc-state]
					   (assoc npc :dead 
						  (if (>= 0 (:hit-points npc))
						    true
						    false))))
				(ref-set npc-state
					 (for [npc @npc-state]
					   (assoc npc :tile
						  (if (>= 0 (:hit-points npc))
						    :dead
						    (:tile npc)))))))

		do-npc-turns (fn [] ;; TODO refactor me to npcs
			       (ref-set npc-state 
					(for [npc @npc-state] 
					  (if (:dead npc)
					    npc
					    (assoc npc :position 
						   (npc/move-non-player (:position npc) @world-state))))))
		
		position (@player-state :position)
		x (first position)
		y (second position)]
	    (do ;; TODO remove this do, make state threaded
	      (move dir)
	      (make-dead)
	      (do-npc-turns)
	      (world/draw-world @world-state 
				@sprite-state
				@player-state
				@npc-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Listener / JFrame / Main 

(def key-map {\a :west
	      \q :northwest
	      \w :north
	      \e :northeast
	      \d :east
	      \c :southeast
	      \x :south
	      \z :southwest})

;; TODO eventually, this will need to communicate more than just a direction, like whether to look
;; at inventory or cast a spell - add these as keys to the map above
(defn gen-keyboard-handler
  [text-area]
  (proxy [KeyListener] []
    (keyPressed [ke] nil)
    (keyReleased [ke] nil)
    (keyTyped [ke] (. text-area (setText (-> ke .getKeyChar key-map do-turn))))))

(defn -main
  [& args]
  (let [frame (JFrame.)
	text-area (JTextArea. max-x max-y)]
    (doto frame
      (.setSize (* 8 40) (+ 25 (* 12 40))) ;;; TODO this is incorrect 
      (.add (doto (JPanel.)
	      (.add (doto text-area
		      (.setEditable false)
		      (.setFont (Font. "Monospaced" (. Font PLAIN) 10))
		      (.setText (world/draw-world 
				 @world-state 
				 @sprite-state 
				 @player-state
				 @npc-state))
		      (.addKeyListener (gen-keyboard-handler text-area))))))
      (.setResizable false)
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))))
