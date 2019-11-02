(ns clojure-ces.example.game
  (:require [clojure.tools.logging :as log]
            [clojure-ces.example.litiengine :as litiengine]
            [clojure-ces.example.graphics :as graphics]
            [clojure-ces.example.asteroid :as asteroid])
  (:import (java.awt Graphics2D))
  (:gen-class))

(def screen-w 600)
(def wrap-border 40)

(def config {:screen-size  [screen-w screen-w]
             :bounding-box [(- wrap-border) (- wrap-border)
                            (+ screen-w wrap-border)
                            (+ screen-w wrap-border)]})

(defonce world (atom (asteroid/init-world config)))

(defn reset-world []
  (swap! world (constantly (asteroid/init-world config))))

(defn draw-scene [^Graphics2D g]
  (graphics/draw-things g world))

(defn -main [& args]
  (log/info "Starting")
  (litiengine/start-engine args draw-scene config)
  (log/info "Game loop running"))
