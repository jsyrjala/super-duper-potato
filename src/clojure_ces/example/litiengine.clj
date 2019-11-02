(ns clojure-ces.example.litiengine
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as jio]
            [clojure-ces.example.input :as input])
  (:import (de.gurkenlabs.litiengine Game GameListener)
           (de.gurkenlabs.litiengine.gui.screens Screen Resolution)
           (java.awt Graphics2D Taskbar)
           (javax.imageio ImageIO))
  )

;; https://stackoverflow.com/questions/38573470/make-a-class-that-extends-a-class-with-overriding-in-clojure
(defn make-screen [name render-fn]
  (proxy [Screen]
         [name]
    (render [^Graphics2D g]
      (render-fn g))))

(defn start-engine [args draw-fn]
  (log/info "start" args)
  (Game/init (into-array String args))
  (input/register-key-handlers)
  (let [window (Game/window)
        icon (ImageIO/read (jio/resource "asteroid.png"))
        taskbar (Taskbar/getTaskbar)]
    (.setIconImage window icon)
    (.setIconImage taskbar icon)
    (.setTitle window "super-duper-potato!")
    (.setResolution window (Resolution/custom 400 400 "4x3")))

  (log/info "init done")
  (Game/addGameListener (reify GameListener
                          (started [_]
                            (log/info "Gamelistener started"))
                          (terminated [_]
                            (log/info "Gamelistener terminated"))
                          ))
  (let [screen (make-screen "draw-asteroids" draw-fn)]
    (.add (Game/screens) screen))

  (Game/start)
  (log/info "start done"))

