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
  {:component/type :controlled})

(defn shooter []
  {:component/type    :shooter
   :shooter/last-shot 0})

(defn aging [now max-age]
  {:component/type   :aging
   :aging/death-time (+ now max-age)
   :aging/birth-time now})

;; entities
(def player (system/create-entity
              [(named "player")
               (controlled)
               (position (vector/vector2 40 30) 0)
               (movement (vector/vector2 0 0) (vector/vector2 0 0))
               (drawable :player)
               (shooter)
               (score 0 3)]))

(defn create-player [pos]
  (system/create-entity
    [(named "player")
     (controlled)
     (position pos 0)
     (movement (vector/vector2 0 0) (vector/vector2 0 0))
     (drawable :player)
     (shooter)
     (score 0 3)]))

;; entities
(def player (system/create-entity
              [(named "player")
               (controlled)
               (position (vector/vector2 40 30) 0)
               (movement (vector/vector2 0 0) (vector/vector2 0 0))
               (drawable :player)
               (shooter)
               (score 0 3)]))

(defn create-asteroid [pos velocity direction angular-velocity]
  (system/create-entity
    [(named "asteroid")
     (position pos direction)
     (movement velocity (vector/vector2 0 0) angular-velocity 2.0)
     (drawable :asteroid)
     ]))

(defn create-bullet [now pos velocity direction]
  (system/create-entity
    [(named "bullet")
     (position pos direction)
     (movement velocity (vector/vector2 0 0) 0 4.0)
     (drawable :bullet)
     (aging now 2500)
     ]))
