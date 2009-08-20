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

(def max-x 20)
(def max-y 20)

(def world-state (ref (world/gen-world max-x max-y)))
(def sprite-state (ref (sprite/gen-sprites @world-state)))
(def npc-state (ref (npc/gen-random-npcs @world-state)))
(def player-state (ref (player/gen-player @world-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Listener / JFrame / Main 

(defn gen-keyboard-handler
  [text-area]
     (proxy [KeyListener] []
       (keyPressed [ke] nil)
       (keyReleased [ke] nil)
       (keyTyped [ke]
		 (dosync (let [do-npc-turns (fn [] 
					      (ref-set npc-state 
						       (for [npc @npc-state] 
							 (assoc npc :position 
								(npc/move-non-player (:position npc) @world-state)))))
			       position (@player-state :position)
			       move (fn [x y] 
				      (alter player-state assoc 
					     :position (player/move-player 
							(@player-state :position)
							@world-state
							x y)))
			       x (first position)
			       y (second position)
			       c (. ke getKeyChar)]
			   (do 
			     (condp = c
			       \a (move x (- y 1))
			       \q (move (- x 1) (- y 1))
			       \w (move (- x 1) y)
			       \e (move (- x 1) (+ y 1))
			       \d (move x (+ y 1)) 
			       \c (move (+ x 1) (+ y 1))  
			       \x (move (+ x 1) y)   
			       \z (move (+ x 1) (- y 1))
			       nil)
			     (do-npc-turns)
			     (. text-area (setText (world/draw-world 
						    @world-state 
						    @sprite-state
						    @player-state
						    @npc-state)))))))))

(defn -main
  [& args]
(let [frame (JFrame.)
      text-area (JTextArea. max-x max-y)]
  (doto frame
    (.setSize (* 8 max-x) (+ 25 (* 18 max-y)))
    (.add (doto (JPanel.)
	    (.add (doto text-area
		    (.setEditable false)
		    (.setFont (Font. "Monospaced" (. Font PLAIN) 12 ))
		    (.setText (world/draw-world 
			       @world-state 
			       @sprite-state 
			       @player-state
			       @npc-state))
		    (.addKeyListener (gen-keyboard-handler text-area))))))
    (.setResizable false)
    (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
    (.setVisible true))))
