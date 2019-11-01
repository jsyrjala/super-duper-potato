(ns clojure-ces.example.graphics
  (:require [clojure-ces.system :as system])
  (:import (java.awt Graphics2D Color Toolkit)
           (de.gurkenlabs.litiengine.graphics RenderEngine)))

(def text (atom "foo2"))

(defn draw-things [^Graphics2D g world]
  (let [entities (:world/entities @world)
        id (first (keys entities))
        entity-1 (entities id)
        position (system/first-component entity-1 :position)
        [^double x ^double y] (:position/position position)
        ;; x 0.0
        ;;y 10.0
        ]
    ;; TODO this is totally in wrong place?
    (swap! world system/game-loop)
    (.setColor g Color/RED)
    (RenderEngine/renderText g (str @text " bar") x y)
    ;; https://stackoverflow.com/questions/18684220/why-is-java-application-running-smoother-when-moving-mouse-over-it-video-includ
    (.sync (Toolkit/getDefaultToolkit))
    ))

