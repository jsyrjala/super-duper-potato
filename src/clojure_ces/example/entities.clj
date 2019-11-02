(ns clojure-ces.example.entities
  (:require [clojure-ces.example.vector :as vector]
            [clojure-ces.system :as system]))


(defn named [name]
  {:component/type :named
   :named/name     name})

(defn position [pos direction]
  {:component/type     :position
   :position/position  pos
   :position/direction direction})

(defn movement
  ([velocity acceleration]
   (movement velocity acceleration 0.0 2.0))
  ([velocity acceleration angular-velocity max-velocity]
   {:component/type            :movement
    :movement/velocity         velocity
    :movement/acceleration     acceleration
    :movement/angular-velocity angular-velocity
    :movement/max-velocity     max-velocity}))

(defn drawable [sprite]
  {:component/type  :drawable
   :drawable/sprite sprite})


(defn score [score lives]
  {:component/type :score
   :score/score    score
   :score/lives    lives})

(defn controlled []
  {:component/type :controlled
   :controlled/thrust false
   :controlled/left false
   :controlled/right false
   :controlled/shoot false})

(defn shooter []
  {:component/type    :shooter
   :shooter/last-shot 0})

(defn aging [now max-age]
  {:component/type   :aging
   :aging/death-time (+ now max-age)
   :aging/birth-time now})

;; This is attached to single entity (player)
(defn collider-handler []
  {:component/type :collider-handler})

;; these are attached to anyting that can collide
(defn collider []
  {:component/type :collider})

(defn health [max-hit-points]
  {:component/type :health
   :health/hit-points max-hit-points})

(defn wrap-around []
  {:component/type :wrap-around})

(defn size [radius]
  {:component/type :size
   :size/radius     (or radius 10)})

(defn flasher []
  {:component/type :flasher
   :flasher/start 0
   :flasher/end 0})

;; entities

(defn create-player [pos]
  (system/create-entity
    [(named :player)
     (controlled)
     (position pos 0)
     (movement (vector/vector2 0 0) (vector/vector2 0 0))
     (drawable :player)
     (shooter)
     (collider-handler)
     (collider)
     (health 1)
     (wrap-around)
     (score 0 3)]))


;; :asteroid :big-asteroid
(defn create-asteroid [pos velocity direction angular-velocity radius]
  (system/create-entity
    [(named :asteroid)
     (position pos direction)
     (size radius)
     (movement velocity (vector/vector2 0 0) angular-velocity 2.0)
     (drawable :asteroid)
     (collider)
     (health (int (+ (/ radius 5) 3)))
     (wrap-around)
     (flasher)
     ]))

(defn create-bullet [now pos velocity direction]
  (system/create-entity
    [(named :bullet)
     (position pos direction)
     (movement velocity (vector/vector2 0 0) 0 4.0)
     (drawable :bullet)
     (aging now 2500)
     (collider)
     (wrap-around)
     ]))

(defn create-particle [now pos velocity direction max-age]
  (system/create-entity
    [(named :particle)
     (position pos direction)
     (movement velocity (vector/vector2 0 0) 0 6.0)
     (drawable :particle)
     (aging now max-age)
     ]))
