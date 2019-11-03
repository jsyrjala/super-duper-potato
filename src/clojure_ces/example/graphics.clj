(ns clojure-ces.example.graphics
  (:require [clojure-ces.system :as system]
            [clojure.tools.logging :as log]
            [clojure-ces.example.vector :as vector])
  (:import (java.awt Graphics2D Color Polygon)
           (java.awt.geom Line2D$Double Rectangle2D$Double)))


(defn draw-screen [^Graphics2D g world]

  )

(defn draw-text [^Graphics2D g world entity]
  (let [[^double x ^double y] (system/component-value entity :position/position)
        ^String text (system/component-value entity :text-display/text)]
    (.setColor g Color/WHITE)
    (.drawString g text x y)
    ))

(defn draw-player [^Graphics2D g world entity]
  (let [[^double x ^double y] (system/component-value entity :position/position)
        score (system/component-value entity :score/current-score)]
    (when x
      (.setColor g Color/YELLOW)
      (let [direction (system/component-value entity :position/direction)
            top [0 -5.0]
            [tx ty] (vector/rotate top direction)
            left [5.0 10.0]
            [lx ly] (vector/rotate left direction)
            right [-5.0 10.0]
            [rx ry] (vector/rotate right direction)]
        (.draw g (new Line2D$Double (+ tx x), (+ ty y), (+ x lx) (+ y ly)))
        (.draw g (new Line2D$Double (+ tx x), (+ ty y), (+ x rx), (+ y ry))))
      (.drawString g (str "Score: " score) 350 10)
      )))

(defn color-flasher [now color-normal flasher]
  (let [start (:flasher/start flasher)
        end (:flasher/end flasher)]
    (if (< end now)
      color-normal
      (let [p (- 1.0 (/ (- now start)
                        (- end start)))
            ip (- 1 p)
            cp (* p 255)
            r (int (+ cp (* ip (.getRed color-normal))))
            g (int (+ cp (* ip (.getGreen color-normal))))
            b (int (+ cp (* ip (.getBlue color-normal))))]
        (Color. r g b)))))

(defn draw-asteroid [^Graphics2D g world entity]
  (let [now (:world/loop-timestamp world)
        flasher-c (system/first-component entity :flasher)
        radius (system/component-value entity :size/radius)
        [^double x ^double y] (system/component-value entity :position/position)
        direction (system/component-value entity :position/direction)
        points [[(- radius) (- radius)]
                [radius (- radius)]
                [radius radius]
                [(- radius) radius]]
        points (map #(vector/rotate % direction) points)
        poly (Polygon.)]
    (doseq [[px py] points]
      (.addPoint poly (+ x px) (+ y py)))
    (.setColor g (color-flasher now Color/RED flasher-c))
    (.draw g poly)
  ))


(defn draw-bullet [^Graphics2D g world entity]
  (let [[^double x ^double y] (system/component-value entity :position/position)
        direction (system/component-value entity :position/direction)
        points [[-0.5 -0.5] [0.5 -0.5] [0.5 0.5] [-0.5 0.5]]
        points (map #(vector/rotate % direction) points)
        poly (Polygon.)]
    (doseq [[px py] points]
      (.addPoint poly (+ x px) (+ y py)))
    (.setColor g Color/CYAN)
    (.draw g poly)
    ))

(defn draw-particle [^Graphics2D g world entity]
  (let [now (double (:world/loop-timestamp world))
        [^double x ^double y] (system/component-value entity :position/position)
        aging (system/first-component entity :aging)
        death-time (double (:aging/death-time aging))
        birth-time (double (:aging/birth-time aging))
        frac (int
               (* 255.0
                  (/ (- death-time now)
                     (- death-time birth-time))))
        color (Color. frac, frac, frac)]
    (.setColor g color)
    (.draw g (Rectangle2D$Double. x y 1.0 1.0))
    ))

(defn draw-entity [^Graphics2D g world entity]
 (let [[^double x ^double y] (system/component-value entity :position/position)]
   (.setColor g Color/RED)
   (.draw g (Rectangle2D$Double. x y 10.0 10.0))
   ))

(def sprites {:player draw-player
              :asteroid draw-asteroid
              :bullet draw-bullet
              :particle draw-particle
              :text draw-text
              :default draw-entity})

(def first-time (atom true))

(defn draw-things [^Graphics2D g world]
  (let [current-world @world
        entities (system/system-managed-entities current-world :drawable-system)]

    (when @first-time
      (log/info "The first text render takes couple of seconds because app is loading fonts")
      (.drawString g " " -10 -10)
      (swap! first-time (constantly false)))

    (draw-screen g current-world)
    (doseq [entity entities]
      (let [drawable (system/first-component entity :drawable)
            sprite (:drawable/sprite drawable)
            draw-fn (get sprites sprite draw-entity)]
        (when draw-fn
          (draw-fn g current-world entity))))

    ;; https://stackoverflow.com/questions/18684220/why-is-java-application-running-smoother-when-moving-mouse-over-it-video-includ
    ;;(.sync (Toolkit/getDefaultToolkit))
    ;; TODO this is totally in wrong place?
    ;; TODO separate thread?
    (swap! world system/game-loop)
    ))

