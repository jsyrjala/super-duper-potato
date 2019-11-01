(ns clojure-ces.example.graphics
  (:require [clojure-ces.system :as system]
            [clojure.tools.logging :as log])
  (:import (java.awt Graphics2D Color Toolkit)
           (de.gurkenlabs.litiengine.graphics RenderEngine)
           (java.awt.geom Line2D$Double Rectangle2D$Double)))

(def text (atom "foo2"))

(defn draw-entity [^Graphics2D g entity]
 (let [^String e-name (:named/name (system/first-component entity :named))
       position (system/first-component entity :position)
       [^double x ^double y] (:position/position position)]
   (when position
     (.setColor g Color/RED)
     (RenderEngine/renderShape g (Rectangle2D$Double. x y 10.0 10.0)))))

(defn draw-things [^Graphics2D g world]
  (let [entities (:world/entities @world)
        id (first (keys entities))
        entity-1 (entities id)
        ^String e-name (:named/name (system/first-component entity-1 :named))
        position (system/first-component entity-1 :position)
        cx 0.0
        cy 0.0
        ;; x 0.0
        ;;y 10.0
        ]
    ;;(log/info entity-1)
    ;; TODO this is totally in wrong place?
    (swap! world system/game-loop)2


    (.setColor g Color/YELLOW)
    (RenderEngine/renderShape g (Rectangle2D$Double. cx cy 10.0 10.0))

    (doseq [id (keys entities)]
      (draw-entity g (entities id)))


    ;; https://stackoverflow.com/questions/18684220/why-is-java-application-running-smoother-when-moving-mouse-over-it-video-includ
    (.sync (Toolkit/getDefaultToolkit))
    ))

