(ns joshhack
  (:gen-class)
  (:import [javax.swing JFrame JTextArea JPanel]
	   [java.awt Font]
	   [java.awt.event KeyListener KeyEvent])
  (:require [joshhack.world :as world]
	    [joshhack.sprite :as sprite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; World State

(def max-x 50)
(def max-y 50)

(def world-state (ref (world/gen-world max-x max-y)))
(def sprite-state (ref (sprite/gen-sprites @world-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Listener / JFrame / Main 

(defn gen-keyboard-handler
  [text-area]
     (proxy [KeyListener] []
       (keyPressed [ke] nil)
       (keyReleased [ke] nil)
       (keyTyped [ke]
		 (dosync (let [player (@sprite-state :player) 
			       move (fn [x y] 
				      (alter sprite-state assoc 
					     :player (sprite/move-player 
						      (@sprite-state :player)
						      @world-state
						      x y))
				      (. text-area (setText (world/draw-world @world-state @sprite-state))))
			       x (first player)
			       y (second player)
			       c (. ke getKeyChar)]
			   (condp = c
			     \a (move x (- y 1))
			     \q (move (- x 1) (- y 1))
			     \w (move (- x 1) y)
			     \e (move (- x 1) (+ y 1))
			     \d (move x (+ y 1)) 
			     \c (move (+ x 1) (+ y 1))  
			     \x (move (+ x 1) y)   
			     \z (move (+ x 1) (- y 1))
			     nil))))))

(defn -main
  [& args]
(let [frame (JFrame.)
      text-area (JTextArea. max-x max-y)]
  (doto frame
    (.setSize (* 8 max-x) (+ 25 (* 18 max-y)))
    (.add (doto (JPanel.)
	    (.add (doto text-area
		    (.setEditable false)
		    (.setFont (Font. "Monospaced" (. Font PLAIN) 14 ))
		    (.setText (world/draw-world @world-state @sprite-state))
		    (.addKeyListener (gen-keyboard-handler text-area))))))
    (.setResizable false)
    (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
    (.setVisible true))))
