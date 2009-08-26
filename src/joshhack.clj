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

(def world-state (ref [(world/gen-world max-x max-y)]))
(def sprite-state (ref [(sprite/gen-sprites (first @world-state))]))
(def npc-state (ref [(npc/gen-random-npcs (first @world-state))]))
(def player-state (ref (player/gen-player (first @world-state))))
(def turns (ref 0))

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


conj  
(def level (:on-level @player-state))

(defn- total-levels [] (count @world-state))

(defn- make-new-level
  []
  (let [new-level (last (alter world-state conj (world/gen-world max-x max-y)))]
    (do (alter sprite-state conj (sprite/gen-sprites new-level))
	(alter npc-state conj (npc/gen-random-npcs new-level))
	new-level)))

(defn- get-level [] (if (> (total-levels) level) 
		      (nth @world-state level) 
		      make-new-level))

(defn- get-sprites [] (nth @sprite-state level))

(defn- get-npcs [] (nth @npc-state level))

(defn values 
  [coll]
  (for [key (keys coll)] (key coll)))

(defn- do-sprites
  [pos]
  (let [sprites (get-sprites)]
  (if (some (fn [x] (= pos x)) (values sprites))
	(do (println sprites)
	    pos)
	pos)))

(defn- change-player-pos
  [pos]
  (alter player-state assoc :position pos))

(defn- move-player
  [dir]
  (let [new-pos 
	(player/get-new-pos 
	 (:position @player-state) 
	 (get-level) 
	 dir)]
    (if (not (npc/some-npc-defending? new-pos (get-npcs)))
      (-> new-pos
	  do-sprites
	  change-player-pos)
      new-pos)))
    
(defn- melee-player 
  [new-pos]
  (let [npcs (get-npcs)]
    (if (= clojure.lang.LazilyPersistentVector (class new-pos))
      (alter npc-state assoc level (npc/receive-attack (:attack @player-state) new-pos npcs))
      npcs)))

(defn- melee-npc
  [npcs]
  (for [{pos :position dead :dead attack :attack :as npc} 
	npcs]
    (let [attacks (if 
		      (and (not dead)
			   (some 
			    (fn [x] (= (:position @player-state) x)) 
			    (npc/melee-range npc (values compass)))) 
;		    (do 
		      (alter player-state assoc :hit-points (- (:hit-points @player-state) attack))
;		      
;		      (println "hit")
;		      (println "NPC")
;		      (println npc)
;		      (println "player")
;		      (println @player-state)
;		      (println "*****"))
		    )]
      (if attacks
	(assoc npc :last-action :attacked)
	npc))))

(defn- move-npc
  [_]
  (let [npcs (get-npcs)]
    (alter npc-state assoc level (npc/move-npcs npcs (get-level) @player-state))))

(defn- draw
  ([] (draw nil))
  ([_] 
     (if (<= (:hit-points @player-state) 0)
       "GAME OVER"
       (world/draw-world 
	(get-level) 
	(get-sprites) 
	@player-state
	(get-npcs)))))

(defn inc-turns [_]
       (ref-set turns (inc @turns)))

(defn- do-turn 
  [dir]
  (if dir
    (dosync (-> dir 
		compass 
		move-player 
		melee-player 
		melee-npc
		move-npc
		inc-turns
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

(defn gen-keyboard-handler
  [text-area]
  (proxy [KeyListener] []
    (keyPressed [ke] nil)
    (keyReleased [ke] nil)
    (keyTyped [ke] (. text-area (setText (-> ke .getKeyChar key-map do-turn))))))

(defn -main
  [& args]
  (let [frame (JFrame.)
	text-area (JTextArea. max-x max-y)
	size-by-vision (int (* (:vision @player-state) 3))
	font-size 10]
    (doto frame
      (.setSize (* font-size size-by-vision) (* font-size size-by-vision)) ;;; TODO this is incorrect 
      (.add (doto (JPanel.)
	      (.add (doto text-area
		      (.setEditable false)
		      (.setFont (Font. "Monospaced" (. Font PLAIN) font-size))
		      (.setText (draw))
		      (.addKeyListener (gen-keyboard-handler text-area))))))
      (.setResizable false)
      (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
      (.setVisible true))))
