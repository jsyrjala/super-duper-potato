(ns clojure-ces.example.asteroid
  (:require [clojure-ces.system :as system]
            [clojure.tools.logging :as log])
  )

;; vectors stuff

(defn vector2 [x y]
  [x y])

(defn number [value]
  (or value 0))

;; TODO very much assumes 2 component vectors
(defn vector-add2 [v1 v2]
  {:pre [(vector? v1)
         (= (count v1) 2)
         (vector? v2)
         (= (count v2) 2)]}
  (let [[x1 y1] (or v1 [0 0])
        [x2 y2] (or v2 [0 0])]
    [(+ (number x1) (number x2))
     (+ (number y1) (number y2))]))

(def graphics)

;;;;
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

(defn gravity-update [world system entity component]
  (let [acceleration (:movement/acceleration component)
        force (:gravity/force system)]
    ;;(log/info "position update" (:entity/id entity) component)
    (assoc component :movement/acceleration
                     (vector-add2 acceleration force))))


(def drawable-system
  (system/create-system
    "Drawable"
    (draw-fn graphics)
    (system/contains-all-components? [:drawable :position])))

(def gravity-system
  (system/create-system
    "Gravity"
    (system/update-component-with :movement gravity-update)
    (system/contains-all-components? [:movement :position])
    {:gravity/force [0 0.001]}))

(defn newton-update [world system entity]
  (let [position-c (system/first-component entity :position)
        movement (system/first-component entity :movement)
        position (:position/position position-c)
        velocity (:movement/velocity movement)
        acceleration (:movement/acceleration movement)
        new-velocity (vector-add2 velocity acceleration)
        new-position (vector-add2 position new-velocity)]
    (-> entity
        (system/update-component :position
                                 (fn newton-pos-up [component]
                                   (assoc component
                                     :position/position new-position)))
        (system/update-component :movement
                                 (fn newton-vel-up [component]
                                   (assoc component
                                     :movement/velocity new-velocity)))
        )))

(def moving-system
  (system/create-system
    "Movable"
    newton-update
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
                 gravity-system
                 moving-system]]
    (-> (reduce system/add-system world systems)
        (system/add-entities
          [player
           asteroid
           bullet
           ]))
    ))


#_(def current-world (atom (init-world)))


#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)