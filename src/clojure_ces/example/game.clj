(ns clojure-ces.example.game
  (:require [clojure-ces.system :as system]
            [clojure-ces.example.graphics-swing :as graphics])
)

;;(defonce graphics (graphics/create-graphics [300 300]))

(def graphics)
(defn draw-fn [graphics]
  (fn [world system entity]
    ;;(graphics/draw-actor graphics entity)
    nil)
  )

(def drawable-system (system/create-system
                       "Drawable"
                       (draw-fn graphics)
                       (system/contains-all-components? [:drawable :position])))

(defn vector2 [x y]
  [x y])

;; entities

;; components
(defn named [name]
  {:component/type :named
   :named/name name})

(defn position [pos direction]
  {:component/type    :position
   :position/position pos
   :position/direction direction})

(defn movement [velocity acceleration]
  {:component/type :movement
   :movement/velocity velocity
   :movement/acceleration acceleration})

(defn score [score lives]
  {:component/type  :score
   :score/score score
   :score/lives lives})

(defn drawable [sprite]
  {:component/type :drawable
   :drawable/sprite sprite})

;; entities
(def player (system/create-entity
              [(named "player")
               (position (vector2 0 0) 0)
               (movement (vector2 0 0) (vector2 0 0))
               (drawable :player)
               (score 0 3)]))

(def asteroid (system/create-entity
                [(named "asteroid")
                 (position (vector2 1 1) 0)
                 (movement (vector2 0 0) (vector2 0 0))
                 (drawable :asteroid)
                 ]))

(def bullet (system/create-entity
              [(named "bullet")
               (position (vector2 1 2) 0)
               (movement (vector2 0.1 0.1) (vector2 0 0))
               (drawable :bullet)]))

(defn init-world []
  (let [world (system/create-world)
        systems [drawable-system]]
    (-> (reduce system/add-system world systems)
        (system/add-entities
          [player
           asteroid
           bullet
           ]))
    ))

