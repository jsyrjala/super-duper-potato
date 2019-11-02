(ns clojure-ces.example.game
  (:require [clojure.tools.logging :as log]
            [clojure-ces.system :as system]
            [clojure-ces.example.litiengine :as litiengine]
            [clojure-ces.example.graphics :as graphics]
            [clojure-ces.example.asteroid :as asteroid])
  (:import (java.awt Graphics2D))
  (:gen-class))

(defonce world (atom (asteroid/init-world)))

(defn reset-world []
  (swap! world (constantly (asteroid/init-world))))

(defn draw-scene [^Graphics2D g]
  (graphics/draw-things g world))

(defn -main [& args]
  (log/info "Starting")
  (litiengine/start-engine args draw-scene)
  (log/info "Game loop running"))
