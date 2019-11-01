(ns clojure-ces.example.graphics
  (:require [clojure-ces.system :as system])
  (:import (java.awt Graphics2D Color)
           (de.gurkenlabs.litiengine.graphics RenderEngine)))

(def text (atom "foo2"))

(defn draw-things [^Graphics2D g world]
  (let [w @world
        entities (:world/entities w)
        id (first (keys entities))
        entity-1 (entities id)
        position (system/first-component entity-1 :position)
        [^double x ^double y] (:position/position position)
        ;; x 0.0
        ;;y 10.0
        ]
    (swap! world system/game-loop)
    (.setColor g Color/RED)
    (RenderEngine/renderText g (str @text " bar") x y)
    ))

