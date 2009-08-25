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

(def compass {:north [0 -1]
	      :south [0 1]
	      :east [1 0]
	      :west [-1 0]
	      :northeast [1 -1]
	      :northwest [-1 -1]
	      :southeast [1 1]
	      :southwest [-1 1]})

(defn- move-player
  [dir]
  (let [new-pos 
	(player/get-new-pos 
	 (:position @player-state) 
	 @world-state 
	 (dir compass))]
    (if (not (npc/some-npc-defending? new-pos @npc-state))
      (alter player-state assoc :position new-pos)
      new-pos)))
    
(defn- melee-player 
  [new-pos]
  (if (= clojure.lang.LazilyPersistentVector (class new-pos))
    (ref-set npc-state (npc/receive-attack (:attack @player-state) new-pos @npc-state))
    @npc-state))

(defn- melee-npc
  [npcs]
  (for [{pos :position dead :dead attack :attack :as npc} 
	npcs]
    (let [attacks (if (and (not dead)
			   (some (fn [x] (= (:position @player-state) x)) (npc/melee-range npc compass)))
		    (do (alter player-state assoc :hit-points (- (:hit-points @player-state) attack))
		      
			(println "hit")
			(println "NPC")
			(println npc)
			(println "player")
			(println @player-state)
			(println "*****")
			
			))]
	  (if attacks
	    (assoc npc :last-action :attacked)
	    npc))))

(defn- move-npc
  [npcs]
  (ref-set npc-state (npc/move-npcs npcs @world-state @player-state)))

(defn- check-end-game
  [_]
  )

(defn- draw
  ([] (draw nil))
  ([_] (world/draw-world @world-state @sprite-state @player-state @npc-state)))

(defn- do-turn 
  [dir]
  (if dir
    (dosync (-> dir 
		move-player 
		melee-player 
		melee-npc
		move-npc
		check-end-game
		draw))
    (draw)))

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
		      (.setText (draw))
		      (.addKeyListener (gen-keyboard-handler text-area))))))
      (.setResizable false)
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))))
