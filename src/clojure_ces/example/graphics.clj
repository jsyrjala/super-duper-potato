(ns clojure-ces.example.graphics
  (:require [clojure-ces.system :as system]
            [clojure.tools.logging :as log]
            [clojure-ces.example.vector :as vector])
  (:import (java.awt Graphics2D Color Toolkit BasicStroke Polygon)
           (java.awt.geom Line2D$Double Rectangle2D$Double)))


(defn draw-player [^Graphics2D g entity]
  (let [position (system/first-component entity :position)
        [^double x ^double y] (:position/position position)]
    (when position
      (.setColor g Color/YELLOW)
      (let [direction (:position/direction position)
            top [0 -5.0]
            [tx ty] (vector/rotate top direction)
            left [5.0 10.0]
            [lx ly] (vector/rotate left direction)
            right [-5.0 10.0]
            [rx ry] (vector/rotate right direction)]
        (.draw g (new Line2D$Double (+ tx x), (+ ty y), (+ x lx) (+ y ly)))
        (.draw g (new Line2D$Double (+ tx x), (+ ty y), (+ x rx), (+ y ry))))
      )))

(defn draw-asteroid [^Graphics2D g entity]
  (let [position (system/first-component entity :position)
        [^double x ^double y] (:position/position position)
        direction (:position/direction position)
        points [[-10.0 -10.0] [10.0 -10.0] [10.0 10.0] [-10.0 10.0]]
        points (map #(vector/rotate % direction) points)
        poly (Polygon.)]
    (doseq [[px py] points]
      (.addPoint poly (+ x px) (+ y py)))
    (.setColor g Color/RED)
    (.draw g poly)
  ))


(defn draw-bullet [^Graphics2D g entity]
  (let [position (system/first-component entity :position)
        [^double x ^double y] (:position/position position)
        direction (:position/direction position)
        points [[-0.5 -0.5] [0.5 -0.5] [0.5 0.5] [-0.5 0.5]]
        points (map #(vector/rotate % direction) points)
        poly (Polygon.)]
    (doseq [[px py] points]
      (.addPoint poly (+ x px) (+ y py)))
    (.setColor g Color/CYAN)
    (.draw g poly)
    ))

(defn draw-entity [^Graphics2D g entity]
 (let [position (system/first-component entity :position)
       [^double x ^double y] (:position/position position)]
   (.setColor g Color/RED)
   (.draw g (Rectangle2D$Double. x y 20.0 30.0))
   ))

(def sprites {:player draw-player
              :asteroid draw-asteroid
              :bullet draw-bullet
              :default draw-entity})


(defn draw-things [^Graphics2D g world]
  (swap! world system/game-loop)

  (let [entities (system/system-managed-entities @world :drawable-system)]
    ;; TODO this is totally in wrong place?

    (doseq [entity entities]
      (let [drawable (system/first-component entity :drawable)
            sprite (:drawable/sprite drawable)
            draw-fn (get sprites sprite draw-entity)]
        (when draw-fn
          (draw-fn g entity))))

    ;; https://stackoverflow.com/questions/18684220/why-is-java-application-running-smoother-when-moving-mouse-over-it-video-includ
    (.sync (Toolkit/getDefaultToolkit))
    ))

