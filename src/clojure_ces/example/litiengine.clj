(ns clojure-ces.example.litiengine
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as jio]
            [clojure-ces.example.asteroid :as asteroid]
            [clojure-ces.system :as system])
  (:import (de.gurkenlabs.litiengine Game GameListener)
           (de.gurkenlabs.litiengine.gui.screens Screen Resolution)
           (java.awt Graphics2D Color Image Taskbar)
           (de.gurkenlabs.litiengine.graphics RenderEngine)
           (javax.imageio ImageIO))
  (:gen-class))

(def text (atom "some text"))

;; https://stackoverflow.com/questions/38573470/make-a-class-that-extends-a-class-with-overriding-in-clojure
(defn make-screen [name render-fn]
  (proxy [Screen]
         [name]
    (render [^Graphics2D g]
      (render-fn g))))

(defonce current-world (atom (asteroid/init-world)))

(defn reset-world []
  (swap! current-world (constantly (asteroid/init-world))))

(defn draw-things [^Graphics2D g]
  (let [w @current-world
        entities (:world/entities w)
        id (first (keys entities))
        entity-1 (entities id)
        position (system/first-component entity-1 :position)
        [^double x ^double y] (:position/position position)
        ;;x 0.0
        ;;y 0.0
        ]
    (swap! current-world system/game-loop)
    (.setColor g Color/RED)
    (RenderEngine/renderText g (str @text "r11t") x y)
    ))

(defn draw-asteroids [^Graphics2D g]
  (draw-things g))

(defn draw-something [^Graphics2D g]
  (let [x 5.0
        y 10.0]
    (.setColor g Color/RED)
    (RenderEngine/renderText g (str @text "ttt") x y)))

(defn start-engine [args]
  (log/info "start" args)
  (Game/init (into-array String args))
  (let [window (Game/window)
        icon (ImageIO/read (jio/resource "asteroid.png"))
        taskbar (Taskbar/getTaskbar)]
    (.setIconImage window icon)
    (.setIconImage taskbar icon)
    (.setTitle window "Asteroids!")
    (.setResolution window (Resolution/custom 400 400 "4x3")))

  (log/info "init done")
  (Game/addGameListener (reify GameListener
                          (started [_]
                            (log/info "Gamelistener started"))
                          (terminated [_]
                            (log/info "Gamelistener terminated"))
                          ))
  (let [screen (make-screen "draw-asteroids" draw-asteroids)]
    (.add (Game/screens) screen)
    )

  (Game/start)
  (log/info "start done")
  )

(defn -main [& args]
  (log/info "Starting")
  (start-engine args)
  (log/info "Game loop running")
  )
