(ns clojure-ces.system)


(def next-id (let [counter (atom 0)]
               (fn [] (swap! counter inc))))

(def add-system)

(defn create-world
  "Create a new, empty world"
  []
  (let [now (System/currentTimeMillis)
        loop-count 0
        world  {:world/id         true
                :world/loop-count loop-count
                :world/loop-times (list [loop-count now])
                :world/entities   {}
                :world/systems    []
                :world/lookups    {}
                }]
    world
    ))

(defn create-system [name update-fn applies?]
  {
   :system/id (next-id)
   :system/name name
   :system/update-fn update-fn
   :system/applies-fn applies?
   }
  )

(defn create-entity [components]
  {:entity/id (next-id)
   :entity/components components})

(defn make-entity-update
  ([entity]
   (make-entity-update [entity] nil nil))
  ([updated-entities created-entities removed-entities]
   {:entity-update/id      true
    :entity-update/updated updated-entities
    :entity-update/created created-entities
    :entity-update/removed removed-entities}))

(defn update-command-for-entity
  "Computes update command by system for entity"
  [world system entity]
  (let [update-fn (or (-> system :system/update-fn)
                      (constantly world))]
    (update-fn world system entity)))

(defn update-command-for-entity-by-id
  "Computes update command by system for entity id"
  [world system entity-id]
  (let [entity (-> world :world/entities (get entity-id))
        _ (when (not entity)
            (println "Missing entity for id. Bug somewhere:" entity-id))
        update-cmd (update-command-for-entity world system entity)]
    update-cmd
    ))

(defn update-entities-in-world [world system updated-entities]
  ;; TODO entity may obtain or remove components? Should check against system applies?
  (if updated-entities
    (update-in world [:world/entities]
               #(apply merge %1 %2)
               (map (fn [entity] {(:entity/id entity) entity})
                    updated-entities))
    world))

(defn create-entities-in-world [world system created-entities]
  ;; TODO add to :world/entities
  ;; add to system lookups
  ;; warn if already exists?
  world)

(defn remove-entities-in-world [world system removed-entities]
  ;; TODO remove from :world/entities
  ;; remove from system lookup
  ;; warn if not exists?
  world)

(defn apply-update-command
  "Applies update command"
  [world system update-command]
  (let [updated-entities (:entity-update/updated update-command)
        created-entities (:entity-update/created update-command)
        removed-entities (:entity-update/removed update-command)]
    (-> world
        (update-entities-in-world system updated-entities)
        (create-entities-in-world system created-entities)
        (remove-entities-in-world system removed-entities))
  ))

(defn apply-system [world system]
  (let [system-id (:system/id system)
        entity-ids (get-in world [:world/lookups system-id :lookup/entity-ids])
        update-commands (doall (map #(update-command-for-entity-by-id world system %) entity-ids))]
    (reduce #(apply-update-command %1 system %2) world update-commands) ))

(defn- start-loop [world]
  (let [now (System/currentTimeMillis)
        loop-count (-> world :world/loop-count inc)
        loop-times (->> world :world/loop-times
                        (take 9)
                        (cons [loop-count now]))]
    ;; (println "LOOP:"  loop-count  "@"  now)
    (-> world
        (update-in [:world/loop-count] (constantly loop-count))
        (update-in [:world/loop-times] (constantly loop-times)
        ))))

(defn game-loop [world]
  (let [new-world (start-loop world)
        systems (-> world :world/systems)
        new-world (reduce #(apply-system %1 %2) new-world systems)]
    new-world))

(defn- add-entities-to-system-lookup [lookups system entities]
  ;; TODO if entities contains dupes, they are not handled correctly
  (let [system-id (-> system :system/id)
        system-lookup (get lookups system-id
                           {:lookup/entity-ids []
                            :lookup/entity-id-set #{}})
        entity-id-set (-> system-lookup :lookup/entity-id-set)
        applies? (-> system :system/applies-fn)
        entities-to-add (->> entities (filter applies?))
        entity-ids (->> entities-to-add
                        (map :entity/id)
                        (remove #(get entity-id-set %)))]
    (cond (seq entity-ids) (-> lookups
                               (update-in [system-id :lookup/entity-ids] #(vec (into % entity-ids)))
                               (update-in [system-id :lookup/entity-id-set] #(set (into % entity-ids))))
          :default lookups)))

(defn- add-entities-to-lookups [lookups systems entities]
  (reduce #(add-entities-to-system-lookup %1 %2 entities) lookups systems))

(defn add-entities
  "Add collection of entities to world."
  [world new-entities]
  (let [entity-map (map (fn [entity] {(-> :entity/id entity) entity}) new-entities)
        systems (-> world :world/systems)]
    (-> world
        (update-in [:world/entities] #(into % entity-map))
        (update-in [:world/lookups] #(add-entities-to-lookups % systems new-entities))
    )))

(defn- remote-entities-from-system-lookup [lookups system entities]
  (let [system-id (-> system :system/id)
        system-lookup (get lookups system-id)
        entity-id-set (-> system-lookup :lookup/entity-id-set)
        to-remove (->> entities
                       (map :entity/id)
                       (filter #(get entity-id-set %)))]
    (cond to-remove
          (-> lookups
              (update-in [system-id :lookup/entity-id-set] #(apply disj % to-remove))
              (update-in [system-id :lookup/entity-ids] #(remove (fn [id] (some #{id} to-remove)) %)))
          :default lookups)))

(defn- remove-entities-from-lookup [lookups systems entities]
  (reduce #(remote-entities-from-system-lookup %1 %2 entities) lookups systems))

(defn remove-entities
  "Remove entities from the world"
  [world entities]
  (let [entity-ids (map :entity/id entities)
        systems (-> world :world/systems)]
    (-> world
        (update-in [:world/lookups] #(remove-entities-from-lookup % systems entities))
        (update-in [:world/entities] #(apply dissoc % entity-ids)))))

(defn add-system
  "Add a new system to the world"
  [world system]
  ;; TODO handle case system already exists
  (let [system-id (-> system :system/id)
        system-name (-> system :system/name)
        entities (-> world :world/entities vals)]
    (-> world
        (update-in [:world/lookups system-id]
                   assoc
                   :lookup/entity-id-set #{}
                   :lookup/entity-ids []
                   :lookup/desc system-name)
        (update-in [:world/systems] #(conj (vec %) system))
        (update-in [:world/lookups] #(add-entities-to-system-lookup % system entities))
        )))

(defn remove-system
  "Remove system from the world"
  [world system]
  (let [system-id (-> system :system/id)]
    (-> world
        (update-in [:world/lookups] dissoc system-id)
        (update-in [:world/systems] #(remove (fn [system]
                                               (= system-id (:system/id system)))
                                             %)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-or-applies?
  "Check if entity contains any of the components."
  [component-types]
  (let [types-set (set component-types)]
    (fn [entity]
      (let [accept (->> entity
                        :entity/components
                        (map :component/type)
                        (filter #(some #{%} types-set)))
            ]
        (boolean (when (seq accept) entity))))))

(defn create-and-applies?
  "Check if entity contains all of the components."
  [component-types]
  (let [types-set (set component-types)]
    (fn [entity]
      (let [accept (->> entity
                        :entity/components
                        (map :component/type)
                        (filter #(some #{%} types-set)))
            ]
        (boolean (= (set accept) types-set))))))

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
          new-components (vec (map #(component-updater %
                                                       (for-component-type component-type)
                                                       component-update-fn)
                                   components))
          new-entity (assoc entity :entity/components new-components)
          update-command (make-entity-update new-entity)]
      update-command
      )))

(defn position-update [world system entity component]
  (let [pos (:position/location component)
        [x y] [(:x pos) (:y pos)]
        new-component (assoc component :position/location {:x (inc x) :y (inc y)})]
    new-component
    ))

(def update-position (update-component-with :position position-update))

(def moving-system (create-system "Moving"
                                  update-position
                                  (create-or-applies? [:position])))

(def drawable-system (create-system "Drawable"
                                    nil
                                    (create-or-applies? [:score]))
  )

(def position {:component/type :position
               :position/location {:x 1 :y 0}})

(def score {:component/type :score
            :score/score 0})


(def entity1 (create-entity [position score]))

(def entity2 (create-entity [position]))
(def entity3 (create-entity [position]))

(def entity4 (create-entity [position]))

(def world0 (create-world))
(def world1 (-> world0
                (add-system moving-system)
                (add-system drawable-system)))

(def world2 (-> world1 (add-entities [entity1]) (add-entities [entity1 entity2])))

(def current-world (atom world2))


(defn n-updates! [n]
    (doseq [i (range 0 n)]
      (swap! current-world game-loop)
      )
  @current-world
  )