(ns clojure-ces.example.asteroid
  (:require [clojure-ces.system :as system]
            [clojure.tools.logging :as log])
  )

;; vectors stuff

(defn vector2 [x y]
  [x y])

;; TODO very much assumes 2 component vectors
(defn vector-add2 [v1 v2]
  (map + v1 v2))

(def graphics)

(defn draw-fn [graphics]
  (fn draw-fn-inner [world system entity]
    ;;(log/info "draw-fn" (:system/name system) (:entity/id entity))
    nil))


(defn position-update [world system entity component]
  (let [position (:position/position component)
        movement (system/first-component entity :movement)
        velocity (:movement/velocity movement)]
    ;(log/info "position update" (:entity/id entity) position velocity)
    (assoc component :position/position
                     (vector-add2 position velocity))))

(def drawable-system
  (system/create-system
    "Drawable"
    (draw-fn graphics)
    (system/contains-all-components? [:drawable :position])))


(def moving-system
  (system/create-system
    "Movable"
    (system/update-component-with :position position-update)
    (system/contains-all-components? [:movement :position])))


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

(defn drawable [sprite]
  {:component/type :drawable
   :drawable/sprite sprite})


(defn score [score lives]
  {:component/type  :score
   :score/score score
   :score/lives lives})

;; entities
(def player (system/create-entity
              [(named "player")
               (position (vector2 0 0) 0)
               (movement (vector2 0 0) (vector2 0 0))
               (drawable :player)
               (score 0 3)]))


(def bullet (system/create-entity
              [(named "bullet")
               (position (vector2 1 2) 0)
               (movement (vector2 0.1 0.1) (vector2 0 0))
               (drawable :bullet)]))


(def asteroid (system/create-entity
                [(named "asteroid")
                 (position (vector2 1 1) 0)
                 (movement (vector2 0 0) (vector2 0 0))
                 (drawable :asteroid)
                 ]))

(defn init-world []
  (let [world (system/create-world)
        systems [drawable-system
                 moving-system]]
    (-> (reduce system/add-system world systems)
        (system/add-entities
          [player
           asteroid
           bullet
           ]))
    ))


(def current-world (atom (init-world)))


(swap! current-world system/game-loop)
(swap! current-world system/game-loop)
(swap! current-world system/game-loop)
(swap! current-world system/game-loop)