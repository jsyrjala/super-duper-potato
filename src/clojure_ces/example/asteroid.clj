(ns clojure-ces.example.asteroid
  (:require [clojure-ces.system :as system]
            [clojure-ces.example.vector :as vector]
            [clojure.tools.logging :as log]
            [clojure-ces.example.input :as input]
            [clojure-ces.example.entities :as entities])
  )

(defn zero-gravity-update [world system entity component]
  (assoc component :movement/acceleration [0.0 0.0]))

(defn point-gravity-update [world system entity component]
  (let [position-c (system/first-component entity :position)
        position (:position/position position-c)
        center (:gravity/center system)
        min-distance (:gravity/min-distance system)
        min-distance 30
        mass (* 50.0 (:gravity/mass system))
        entity-mass 1.0
        dist (max min-distance (vector/distance position center))
        force (/ (* entity-mass mass) (* dist dist))
        direction (vector/add position (vector/negate center))
        unit-direction (vector/normalize direction)
        change (vector/clamp (vector/negate (vector/scale unit-direction force)) 5.0)]
    (assoc component :movement/acceleration change)))

(defn constant-gravity-update [world system entity component]
  (let [force (:gravity/constant-force system)]
    (assoc component :movement/acceleration force)))

(defn gravity-update [world system entity component]
  (zero-gravity-update world system entity component)
  ;;(constant-gravity-update world system entity component)
  ;;(point-gravity-update world system entity component)
  )


(def drawable-system
  (system/create-system
    :drawable-system
    nil
    (system/contains-all-components? [:drawable :position])))

(def gravity-system
  (system/create-system
    :gravity-system
    (system/update-component-with :movement #(gravity-update %1 %2 %3 %4))
    (system/contains-all-components? [:movement :position])
    {:gravity/constant-force [0 0.1]
     :gravity/center         [200.0 200.0]
     :gravity/min-distance   2
     :gravity/mass           1.0}))

(defn newton-update [world system entity]
  (let [position-c (system/first-component entity :position)
        movement (system/first-component entity :movement)
        position (:position/position position-c)
        direction (:position/direction position-c)
        velocity (:movement/velocity movement)
        angular-velocity (:movement/angular-velocity movement)
        acceleration (:movement/acceleration movement)
        new-velocity (vector/clamp (vector/add velocity acceleration)
                                   2.0)
        new-position (vector/add position new-velocity)
        new-direction (+ direction angular-velocity)
        ]
    (-> entity
        (system/update-component :position
                                 (fn newton-pos-up [component]
                                   (assoc component
                                     :position/position new-position
                                     :position/direction new-direction)))
        (system/update-component :movement
                                 (fn newton-vel-up [component]
                                   (assoc component
                                     :movement/velocity new-velocity)))
        )))

(def moving-system
  (system/create-system
    :moving-system
    #(newton-update %1 %2 %3)
    (system/contains-all-components? [:movement :position])))


(defn keyboad-controller [world system entity]
  (let [position-c (system/first-component entity :position)
        movement-c (system/first-component entity :movement)
        acceleration (:movement/acceleration movement-c)
        keys @input/keys-down
        space (keys 32)
        left (keys 37)
        right (keys 39)
        up (keys 38)
        position (:position/position position-c)
        direction (:position/direction position-c)
        speed 0.05
        new-direction (cond (and left right) direction
                            left (- direction speed)
                            right (+ direction speed)
                            :else direction)
        new-accel (cond up (vector/add acceleration
                                       (vector/scale
                                         (vector/rotate [0.0 -1.0] new-direction)
                                         0.05))
                        :else acceleration)
        new-entity (-> entity
                       (system/update-component :position
                                                #(assoc %
                                                   :position/direction new-direction))
                       (system/update-component :movement
                                                #(assoc %
                                                   :movement/acceleration new-accel))

                       )
        bullet (when space
                 (entities/create-bullet position [0 0] direction))
        ]
      (system/make-entity-update new-entity bullet nil)
    ))

(def keyboard-system
  (system/create-system
    :keyboard-system
    #(keyboad-controller %1 %2 %3)
    (system/contains-all-components? [:controlled :shooter :position])))

(defn wrap-around-update [world system entity]
  (let [position-c (system/first-component entity :position)
        position (:position/position position-c)
        bounding-box (:wrap-around/bounding-box system)
        new-position (vector/wrap-around position bounding-box)]
    (system/update-component entity :position
                             #(assoc %
                                :position/position new-position))))

(def wrap-around-system
  (system/create-system
    :wrap-around-system
    #(wrap-around-update %1 %2 %3)
    (system/contains-all-components? [:position])
    {:wrap-around/bounding-box [50.0 50.0 300.0 300.0]}))

(defn named [name]
  {:component/type :named
   :named/name     name})

(defn position [pos direction]
  {:component/type     :position
   :position/position  pos
   :position/direction direction})

(defn movement
  ([velocity acceleration]
   (movement velocity acceleration 0.0))
  ([velocity acceleration angular-velocity]
   {:component/type            :movement
    :movement/velocity         velocity
    :movement/acceleration     acceleration
    :movement/angular-velocity angular-velocity}))

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

;; entities
(def player (system/create-entity
              [(named "player")
               (controlled)
               (position (vector/vector2 40 30) 0)
               (movement (vector/vector2 0 0) (vector/vector2 0 0))
               (drawable :player)
               (shooter)
               (score 0 3)]))


(def bullet (system/create-entity
              [(named "bullet")
               (position (vector/vector2 1 2) 0)
               (movement (vector/vector2 0.1 0.1) (vector/vector2 0 0))
               (drawable :bullet)]))


(defn rand-pos []
  [(+ 20 (rand-int 350)) (+ 20 (rand-int 350))])

(def entities
  [(entities/create-asteroid (rand-pos) [0.1 0.1] 7 0.01 )
   (entities/create-asteroid (rand-pos) [-0.1 0.01] 4 0.02)
   (entities/create-asteroid (rand-pos) [-0.05 -0.01] 0 0.02)
   (entities/create-asteroid (rand-pos) [0.05 -0.01] 1 0.03)

   bullet

   player
   ])

(defn init-world []
  (let [world (system/create-world)
        systems [gravity-system
                 keyboard-system
                 drawable-system
                 moving-system
                 wrap-around-system
                 ]]
    (-> (reduce system/add-system world systems)
        (system/add-entities
          entities))
    ))


#_(def current-world (atom (init-world)))


#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)
#_(swap! current-world system/game-loop)