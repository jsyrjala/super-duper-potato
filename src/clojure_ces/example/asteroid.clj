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
        max-velocity (:movement/max-velocity movement)
        new-velocity (vector/clamp (vector/add velocity acceleration)
                                   max-velocity)
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
        r-key (keys 52)
        position (:position/position position-c)
        direction (:position/direction position-c)
        speed 0.06
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
                       (system/update-component :shooter
                                                #(assoc %
                                                   :shooter/shoots (boolean space)))
                       (system/update-component :controlled
                                                #(assoc %
                                                   :controlled/left (boolean left)
                                                   :controlled/right (boolean right)
                                                   :controlled/thrust (boolean up)
                                                   :controlled/shoot (boolean space)))
                       )
        ]
    new-entity
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

(defn wrap-around-system [config]
  (system/create-system
    :wrap-around-system
    #(wrap-around-update %1 %2 %3)
    (system/contains-all-components? [:position :wrap-around])
    {:wrap-around/bounding-box (:bounding-box config)}))

(defn shooting-update [world system entity]
  (let [shooter-c (system/first-component entity :shooter)
        shoots (:shooter/shoots shooter-c)
        now (:world/loop-timestamp world)
        last-shot (:shooter/last-shot shooter-c)
        position-c (system/first-component entity :position)
        movement-c (system/first-component entity :movement)
        position (:position/position position-c)
        direction (:position/direction position-c)
        velocity (:movement/velocity movement-c)
        b-rel-velocity (vector/scale (vector/rotate [0 -1] direction)
                                     2)
        b-velocity (vector/add velocity
                               b-rel-velocity)]
    (if (and shoots
             (> (- now last-shot) 200))
      (let [bullet (entities/create-bullet now position b-velocity direction)
            new-entity (system/update-component entity :shooter
                                                #(assoc %
                                                   :shooter/last-shot now))]
        (system/make-entity-update new-entity bullet nil))
      entity)))

(def shooting-system
  (system/create-system
    :shooting-system
    #(shooting-update %1 %2 %3)
    (system/contains-all-components? [:shooter])
    ))

(defn aging-update [world system entity]
  (let [aging-c (system/first-component entity :aging)
        now (:world/loop-timestamp world)
        death-time (:aging/death-time aging-c)]
    (if (< now death-time)
      entity
      (system/make-entity-update nil nil entity)
      )))

(def aging-system
  (system/create-system
    :aging-system
    #(aging-update %1 %2 %3)
    (system/contains-all-components? [:aging])
    ))

(defn engine-update [world system entity]
  (let [controlled-c (system/first-component entity :controlled)
        thrust (:controlled/thrust controlled-c)]
    (if (not thrust)
      entity
      (let [now (:world/loop-timestamp world)
            position-c (system/first-component entity :position)
            movement-c (system/first-component entity :movement)
            direction (:position/direction position-c)
            position (:position/position position-c)
            position (vector/add position (vector/rotate [0 8] direction))
            velocity (:movement/velocity movement-c)
            particle-dir (+ direction (rand 0.3) -0.15)
            b-rel-velocity (vector/scale
                             (vector/rotate [0 1]
                                            particle-dir)
                             2)
            b-velocity (vector/add velocity
                                   b-rel-velocity)

            particle (entities/create-particle now position b-velocity direction
                                               (rand-int 300))]
        (system/make-entity-update entity particle nil))
      )))

(def engine-system
  (system/create-system
    :engine-system
    #(engine-update %1 %2 %3)
    (system/contains-all-components? [:controlled :position])
    ))


(defn get-entity-name [entity]
  (:named/name (system/first-component entity :named)))

(defn register-hit [now damage entity]
  (let [health-c (system/first-component entity :health)
        hitpoints (:health/hit-points health-c)]
    (-> entity
        (system/update-component :health
                                 #(assoc %
                                    :health/hit-points (- hitpoints damage)))
        (system/update-component :flasher
                                 #(assoc %
                                    :flasher/start now
                                    :flasher/end (+ now 500)))
        )))

(defn collider-handler-update [world system player]
  (let [now (:world/loop-timestamp world)
        entities (system/system-managed-entities world :collider-system)
        groups (group-by get-entity-name entities)
        bullets (groups :bullet)
        asteroids (groups :asteroid)
        collisions (for [bullet bullets]
                     (for [asteroid asteroids]
                       (let [size-c (system/first-component asteroid :size)
                             radius (:size/radius size-c)
                             b-pos-c (system/first-component bullet :position)
                             b-pos (:position/position b-pos-c)
                             a-pos-c (system/first-component asteroid :position)
                             a-pos (:position/position a-pos-c)]
                         (when (< (vector/distance b-pos a-pos) radius)
                           {:bullet bullet :asteroid asteroid}))
                ))
        player-collisions (for [asteroid asteroids]
                            (let [size-c (system/first-component asteroid :size)
                                  radius (:size/radius size-c)
                                  a-pos-c (system/first-component asteroid :position)
                                  a-pos (:position/position a-pos-c)
                                  p-pos-c (system/first-component player :position)
                                  p-pos (:position/position p-pos-c)]
                              (when (< (vector/distance p-pos a-pos) (+ radius 5))
                                {:player player :asteroid asteroid})
                              ))
        collisions (concat collisions player-collisions)
        collisions (filter identity (flatten collisions))]
    (if (seq collisions)
      (let [bullets-to-remove (filter identity (map :bullet collisions))
            asteroids-to-update (->> collisions
                                     (map :asteroid)
                                     (filter identity)
                                     (map #(register-hit now 1 %)))

            player-hit (->> collisions
                            (map :player)
                            (filter identity)
                            first

                            )
            new-player (if player-hit
                         (register-hit now 1 player)
                         player)

            score-c (system/first-component player :score)
            score (-> score-c :score/current-score)
            top-score (-> score-c :score/top-score)

            hits (count bullets-to-remove)
            new-player (system/update-component new-player :score
                                                #(assoc %
                                                   :score/current-score (+ score hits)
                                                   :score/top-score (max (+ score hits) top-score)))
            ]
        (system/make-entity-update new-player asteroids-to-update bullets-to-remove))
      player)))

;; TODO very ugly
;; attached to the player
(def collider-handler-system
  (system/create-system
    :collider-handler-system
    #(collider-handler-update %1 %2 %3)
    (system/contains-all-components? [:collider-handler])
    ))


;; this is no-op and used as marker to mark things
;; that can collide
(def collider-system
  (system/create-system
    :collider-system
    (fn [_ _ entity] entity)
    (system/contains-all-components? [:collider])
    ))


(defn create-random-particle [now pos]
  (let [direction (rand 20.0)
        velocity (vector/scale
                   (vector/rotate [1 0] direction)
                   (rand 3.0))
        age (rand-int 1000)]
    (entities/create-particle now pos velocity direction age)))

(defn create-new-asteroid [pos radius]
  (let [direction (rand 20.0)
        velocity (vector/scale
                   (vector/rotate [1 0] direction)
                   (rand 1))
        angular-velocity (- (rand 0.06) 0.03)]
    (entities/create-asteroid pos velocity direction angular-velocity radius)
    ))

(defn create-child-asteroids [asteroid]
  (let [pos-c (system/first-component asteroid :position)
        pos (:position/position pos-c)
        size-c (system/first-component asteroid :size)
        radius (or (:size/radius size-c) 10)]
    (cond (< radius 11) [(create-new-asteroid [-15 -15] 20.0)]
          (< radius 16) (map (fn [_] (create-new-asteroid pos 10.0)) (range 4))
          (< radius 21) (map (fn [_] (create-new-asteroid pos 15.0)) (range 3))
          (< radius 31) (map (fn [_] (create-new-asteroid pos 20.0)) (range 2))
          :else (map (fn [_] (create-new-asteroid pos 30.0)) (range 2))
          )))

(defn health-update [world system entity]
  (let [now (:world/loop-timestamp world)
        health-c (system/first-component entity :health)
        pos-c (system/first-component entity :position)
        asteroid-spawner (system/first-component entity :asteroid-spawner)
        game-stopper (system/first-component entity :game-stopper)
        pos (:position/position pos-c)
        hitpoints (:health/hit-points health-c)
        ]
    (if (< hitpoints 1)
      (let [particles (map (fn [_] (create-random-particle now pos))
                           (range 30))
            asteroids (when asteroid-spawner (create-child-asteroids entity))
            text (when game-stopper [(entities/create-text-display now [20 20] "You died! You suck! Press R to retry.")])
            new-entities (concat particles asteroids text)]
        (system/make-entity-update nil new-entities entity))
      entity)))

(def health-system
  (system/create-system
    :health-system
    #(health-update %1 %2 %3)
    (system/contains-all-components? [:health])))

(defn rand-pos [[max-x max-y]]
  [(rand-int max-x) (rand-int max-y)])

(defn rand-velocity [max-speed]
  (let [dir (rand 20.0)
        velocity (vector/rotate [0 1] dir)
        velocity (vector/scale velocity (rand max-speed))]
    velocity))

(defn init-entities [config]
  (let [screen-size (:screen-size config)
        slow 0.5
        medium 1.0
        fast 1.5]
    [(entities/create-asteroid (rand-pos screen-size) (rand-velocity slow) 7 -0.005 20.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity slow) 7 -0.005 20.0)

     (entities/create-asteroid (rand-pos screen-size) (rand-velocity medium) 7 0.01 15.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity medium) 4 0.03 15.0)

     (entities/create-asteroid (rand-pos screen-size) (rand-velocity fast) 0 -0.03 10.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity fast) 1 0.03 10.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity fast) 1 0.03 10.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity fast) 1 0.03 10.0)
     (entities/create-asteroid (rand-pos screen-size) (rand-velocity fast) 1 0.03 10.0)

     (entities/create-player (vector/scale screen-size 0.5))
     (entities/create-score-holder)
     ]))

(defn init-world [config]
  (let [world (system/create-world)
        systems [gravity-system
                 keyboard-system
                 engine-system
                 shooting-system
                 drawable-system
                 moving-system
                 collider-system
                 collider-handler-system
                 (wrap-around-system config)
                 aging-system
                 health-system
                 ]]
    (-> (reduce system/add-system world systems)
        (system/add-entities
          (init-entities config)))
    ))
