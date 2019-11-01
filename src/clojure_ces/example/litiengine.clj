(ns clojure-ces.example.litiengine
  (:require [clojure.tools.logging :as log])
  (:import (de.gurkenlabs.litiengine Game GameListener)
           (de.gurkenlabs.litiengine.gui.screens Screen Resolution)
           (java.awt Graphics2D Color)
           (de.gurkenlabs.litiengine.graphics RenderEngine)
           )
  (:gen-class))

(def text (atom "some text"))

;; https://stackoverflow.com/questions/38573470/make-a-class-that-extends-a-class-with-overriding-in-clojure
(defn make-screen [name render-fn]
  (proxy [Screen]
         [name]
    (render [^Graphics2D g]
      (render-fn g))))


(defn draw-something [^Graphics2D g]
  (let [x 5.0
        y 10.0]
    (.setColor g Color/RED)
    (RenderEngine/renderText g (str @text "ttt") x y)))

(defn start-engine [args]
  (log/info "start" args)
  (Game/init (into-array String args))
  (let [window (Game/window)]
    (.setTitle window "Perkele!")
    (.setResolution window (Resolution/custom 400 400 "4x3")))

  (log/info "init done")
  (Game/addGameListener (reify GameListener
                          (started [_]
                            (log/info "Gamelistener started"))
                          (terminated [_]
                            (log/info "Gamelistener terminated"))
                          ))
  (let [screen (make-screen "draw-something" draw-something)]
    (.add (Game/screens) screen))

  (Game/start)
  (log/info "start done")
  )

(defn -main [& args]
  (log/info "Starting")
  (start-engine args)
  (log/info "Game loop running")
  )
