(ns clojure-ces.example.asteroid
  (:require [clojure-ces.system :as system]
            [clojure-ces.example.vector :as vector]
            [clojure.tools.logging :as log])

  )

;; vectors stuff

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
                     (vector/add position velocity))))



(defn point-gravity-update [world system entity component]
  (let [position-c (system/first-component entity :position)
        position (:position/position position-c)
        velocity (:movement/velocity component)
        speed (vector/length velocity)
        center (:gravity/center system)
        min-distance (:gravity/min-distance system)
        e-name (:named/name (system/first-component entity :named))
        mass (* 100 (:gravity/mass system))
        entity-mass 1.0
        dist (max min-distance (vector/distance position center))
        force (/ (* entity-mass mass) (* dist dist))
        direction (vector/add position (vector/negate center))
        unit-direction (vector/normalize direction)
        change (vector/clamp (vector/negate (vector/scale unit-direction force)) 5.0)
        ]
    (when (= e-name "player")
      (log/info e-name "gravity" position "s=" speed "d=" dist "f=" force change))
    (assoc component :movement/acceleration change)))

(defn constant-gravity-update [world system entity component]
  (let [acceleration (:movement/acceleration component)
        force (:gravity/constant-force system)]
    ;;(log/info "position update" (:entity/id entity) component)
    (assoc component :movement/acceleration force)))

(defn gravity-update [world system entity component]
  ;;component
  (constant-gravity-update world system entity component)
  ;; (point-gravity-update world system entity component)
  )


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
    {:gravity/constant-force [0 0.1]
     :gravity/center         [0.0 0.0]
     :gravity/min-distance   10
     :gravity/mass           1.0}))

(defn newton-update [world system entity]
  (let [position-c (system/first-component entity :position)
        movement (system/first-component entity :movement)
        position (:position/position position-c)
        velocity (:movement/velocity movement)
        acceleration (:movement/acceleration movement)
        ;; _ (log/info "pos" position "vel" velocity "acc" acceleration)
        new-velocity (vector/add velocity acceleration)
        new-position (vector/add position new-velocity)
        ;;_ (log/info "pos" new-position "vel" new-velocity)
        ]
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
   :named/name     name})

(defn position [pos direction]
  {:component/type     :position
   :position/position  pos
   :position/direction direction})

(defn movement [velocity acceleration]
  {:component/type        :movement
   :movement/velocity     velocity
   :movement/acceleration acceleration})

(defn drawable [sprite]
  {:component/type  :drawable
   :drawable/sprite sprite})


(defn score [score lives]
  {:component/type :score
   :score/score    score
   :score/lives    lives})

;; entities
(def player (system/create-entity
              [(named "player")
               (position (vector/vector2 30 30) 0)
               (movement (vector/vector2 0 0) (vector/vector2 0 0))
               (drawable :player)
               (score 0 3)]))


(def bullet (system/create-entity
              [(named "bullet")
               (position (vector/vector2 1 2) 0)
               (movement (vector/vector2 0.1 0.1) (vector/vector2 0 0))
               (drawable :bullet)]))


(def asteroid (system/create-entity
                [(named "asteroid")
                 (position (vector/vector2 1 1) 0)
                 (movement (vector/vector2 0 0) (vector/vector2 0 0))
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