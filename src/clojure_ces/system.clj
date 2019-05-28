(ns clojure-ces.system)



(def next-entity-id (let [counter (atom 0)]
                      (fn [] (swap! counter inc))))


(def next-system-id (let [counter (atom 0)]
                      (fn [] (swap! counter inc))))

(defn create-entity [components]
  {:entity/id (next-entity-id)
   :entity/components components})

(defn create-applies?
  "Check if entity contains any of the components."
  [component-types]
  (fn [entity]
    (let [accept (->> entity
                      :entity/components
                      (map :component/type)
                      (filter #(some #{%} component-types)))
          ]
      (boolean (when (seq accept) entity)))))


(defn create-system [name update-fn applies?]
  {
   :system/id (next-entity-id)
   :system/name name
   :system/update-fn update-fn
   :system/applies-fn applies?
   :system/entity-ids  []
   :system/entity-id-set #{}
   }
  )


(defn create-system2 [name update-fn applies-components]
  (create-system name update-fn (create-applies? applies-components)))

(defn update-command [result old-entity]
  (cond (-> result :entity/id) {:system/entity result}
        (-> result :system/entity) result
        :default {:system/entity old-entity}))

(defn update-entity [world system entity]
  (let [update-fn (-> system :system/update-fn)]
    (update-fn world system entity)))

(defn update-entity-by-id [world system entity-id]
  (println "update-entity" "id:" entity-id)
  (let [entity (-> world :world/entities (get entity-id))]
    (when (not entity)
      (println "Missing entity for id. Bug somewhere:" entity-id))
    (let [x (update-entity world system entity)]
      (println "update-fn res" x)
      x)
    ))

(defn apply-system [world system]
  (let [entity-ids (-> system :system/entity-ids)
        update-commands (map #(update-entity-by-id world system %) entity-ids)]
    (println "update-comd" update-commands (nil? update-commands) entity-ids)
    (cond (nil? update-commands) world
          :default world)
    )
  )

(defn game-loop [world]
  (let [systems (-> world :world/systems)
        _ (println "game-loop systems" (count systems))
        ;; TODO
        new-world (map #(apply-system world %) (-> world :world/systems))
        ;;new-world (reduce #(apply-system world %) (-> world :world/systems))
        ]
    new-world))

(defn- add-entities-to-system [system entities]
  ;; TODO adding entities to system is not good, they should not be part of a system?
  ;;      makes testing systems difficult
  ;;
  ;; TODO if entities contains dupes, they are not handled correctly
  ;; if system already contains entity, it is filtered correctly
  (let [applies? (-> system :system/applies-fn)
        entity-id-set (-> system :system/entity-id-set)
        entities-to-add (->> entities (filter applies?))
        entity-ids (->> entities-to-add
                        (map :entity/id)
                        (remove #(get entity-id-set %)))]
    (cond (not (seq entity-ids)) system
          :default (-> system
                       (update-in [:system/entity-ids] #(into % entity-ids))
                       (update-in [:system/entity-id-set] #(into % entity-ids))))))

(defn add-entities
  "Add collection of entities to world."
  [world new-entities]
  (let [entity-map (map (fn [entity] {(-> :entity/id entity) entity}) new-entities)]
    (-> world
        (update-in [:world/entities] #(into % entity-map))
        (update-in [:world/systems] #(map (fn [system] (add-entities-to-system system new-entities)) %))
    )))


(defn- remove-entities-from-system [system entity-ids]
  (let [entity-id-set (-> system :system/entity-id-set)
        to-remove (->> entity-ids
                       (filter #(get entity-id-set %)))]
    (cond (not (seq to-remove)) system
          :default (-> system
                       (update-in [:system/entity-id-set] #(apply disj % to-remove))
                       (update-in [:system/entity-ids] #(remove (fn [id] (some #{id} to-remove)) %))))))

(defn remove-entities [world entities]
  (let [entity-ids (map :entity/id entities)]
    (-> world
        (update-in [:world/systems] #(map (fn [system] (remove-entities-from-system system entity-ids)) %))
        (update-in [:world/entities] #(apply dissoc % entity-ids)))))

(defn create-world [systems]
  {
   :world/entities {}
   :world/systems systems
   }
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn component-updater [component applies? update-fn]
  (if (applies? component)
    (update-fn component)
    component))

(defn for-component-type [type]
  (fn [component]
    (= (:component/type component) type)))

(defn update-component-with [component-type update-fn]
  (fn [world system entity]
    (let [components (-> entity :entity/components)
          component-update-fn (fn [component]
                                (update-fn world system entity component))
          new-components (map #(component-updater %
                                                  (for-component-type component-type)
                                                  component-update-fn)
                              components)]
      (assoc entity :entity/components new-components))))

(defn position-update [world system entity component]
  (let [pos (:position/location component)
        [x y] [(:x pos) (:y pos)]]
    (assoc position :position/location {:x (inc x) :y (inc y)})))

(def update-position (update-component-with :position position-update))

(def moving-system (create-system "Moving"
                                  update-position
                                  (create-applies? [:position])))

(def drawable-system (create-system "Drawable"
                                  update-position
                                  (create-applies? [:score]))
  )

(def position {:component/type :position
               :position/location {:x 1 :y 0}})

(def score {:component/type :score
            :score/score 0})


(def entity1 (create-entity [position score]))

(def entity2 (create-entity [position]))
(def entity3 (create-entity [position]))

(def entity4 (create-entity [position]))

(def world (create-world [moving-system drawable-system]))

(def world2 (-> world (add-entities [entity1]) (add-entities [entity1 entity2])))