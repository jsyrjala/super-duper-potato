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

(defn update-entity [world system entity-id]
  (let [update-fn (-> system :system/update-fn)
        entity (-> world :world/entities entity-id)]
    (when entity
      (print "Missing entity for id. Bug somewhere:" entity-id))
    (update-fn world system entity)
    ))

(defn apply-system [world system]
  (let [entity-ids (-> system :system/entity-ids)
        update-commands (map #(update-entity world system %) entity-ids)]
    )
  )

(defn game-loop [world]
  (let [entities (-> :system/entities world)
        new-world (reduce #(apply-system world %) (-> world :world/systems))]
    nil))

(defn- add-entities-to-system [system entities]
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

(defn update-position [world system entity]
  (let [pos (:position/location entity)
        [x y] [(:x pos) (:y pos)]
        new-pos (assoc pos :x (inc x) :y (inc y))]
    (update-in entity [:position/location] new-pos)))


(def moving-system (create-system "Moving"
                                  update-position
                                  (create-applies? [:position]))
  )

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

