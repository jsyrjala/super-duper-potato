(ns clojure-ces.system
  (:require [clojure.tools.logging :as log]))


(def next-id
  (let [counter (atom 0)]
    (fn [] (swap! counter inc))))

(defn create-world
  "Create a new, empty world"
  []
  (let [now (System/currentTimeMillis)
        loop-count 0
        world {:world/id         true
               :world/loop-count loop-count
               :world/loop-times (list [loop-count now])
               :world/entities   {}
               :world/systems    []
               :world/lookups    {} }]
    world))

(defn create-system
  "Create a system
  update-fn: computes update for given entity
  applies-fn: checks if given entity is managed by this system"
  ([name update-fn applies-fn]
   (create-system name update-fn applies-fn {})
   )
  ([name update-fn applies-fn data]
   (merge
     {:system/id         (next-id)
      :system/name       name
      :system/update-fn  (or update-fn
                             (fn [world system entity]
                               entity))
      :system/applies-fn applies-fn}
     data)))

(defn- to-vec [value]
  (cond (nil? value) nil
        (seq? value) (vec value)
        (vector? value) value
        :default [value]))

(defn create-entity [components]
  {:entity/id         (next-id)
   :entity/components (to-vec components)})

(defn make-entity-update
  ([entity]
   (make-entity-update entity nil nil))
  ([updated-entities created-entities removed-entities]
   {:entity-update/id      true
    :entity-update/updated (to-vec updated-entities)
    :entity-update/created (to-vec created-entities)
    :entity-update/removed (to-vec removed-entities) }))

;; Add and remove entities

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

;; Add and remove systems

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

;; Game loop

(defn update-command-for-entity
  "Computes update command by system for entity"
  [world system entity]
  (let [update-fn (or (-> system :system/update-fn)
                      (constantly world))
        update-command (update-fn world system entity)]
    (cond
      (nil? update-command) nil
      ;; returned directly an updated entity
      (update-command :entity/id) (make-entity-update update-command)
      ;; assumed to contain structure created by make-entity-update
      :default update-command) ))

(defn update-command-for-entity-by-id
  "Computes update command by system for entity id"
  [world system entity-id]
  (let [entity (-> world :world/entities (get entity-id))
        _ (when (not entity)
            (println "Missing entity for id. Bug somewhere:" entity-id))
        update-cmd (update-command-for-entity world system entity)]
    update-cmd ))

(defn update-entities-in-world [world system updated-entities]
  ;; TODO Does not support adding/removing components. Lookups for systems are not updated.
  ;; TODO check against old version to see if the components have changed and then run system applies?
  (if (seq updated-entities)
    (update-in world [:world/entities]
               #(apply merge %1 %2)
               (map (fn [entity] {(:entity/id entity) entity})
                    updated-entities))
    world))

(defn create-entities-in-world [world system created-entities]
  ;; warn if already exists?
  (if (seq created-entities)
    (add-entities world created-entities)
    world))

(defn remove-entities-in-world [world system removed-entities]
  ;; warn if not exists?
  (if (seq removed-entities)
    (remove-entities world removed-entities)
    world))

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

;; helper functions

(defn contains-any-components?
  "Check if entity contains any of the components."
  [component-types]
  (let [types-set (set component-types)]
    (fn has-component [entity]
      (let [accept (->> entity
                        :entity/components
                        (map :component/type)
                        (filter #(some #{%} types-set)))
            ]
        (boolean (when (seq accept) entity))))))

(defn contains-all-components?
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

(defn first-component
  "Return the first component of matching type"
  [entity type]
  (->> (-> entity :entity/components)
       (filter (for-component-type type))
       first))

(defn components-updater [components component-type update-fn]
  (vec (map #(component-updater %
                                (for-component-type component-type)
                                update-fn)
            components)))

(defn update-component [entity component-type update-fn]
  (let [components (-> entity :entity/components)
        new-components (vec (map #(component-updater %
                                                     (for-component-type component-type)
                                                     update-fn)
                                 components))]
        (assoc entity :entity/components new-components)))

(defn update-component-with
  "Creates a function to update contents of a single component."
  [component-type update-fn]
  (fn [world system entity]
    (let [components (-> entity :entity/components)
          component-update-fn (fn [component]
                                (update-fn world system entity component))
          new-components (components-updater components component-type component-update-fn)
          new-entity (assoc entity :entity/components new-components)
          update-command (make-entity-update new-entity)]
      (if update-command
        update-command
        (do
          (log/warn "update-with-command: returned nil update command")
          (make-entity-update entity)))
      )))

(defn system-managed-entities [world system-name]
  (let [systems (:world/systems world)
        system (->> systems
                    (filter #(= (:system/name %) system-name))
                    first)
        _ (when (not system)
            (log/warn "system-managed-entities Can't find system-name" system-name))
        system-id (:system/id system)
        entity-ids (get-in world [:world/lookups system-id :lookup/entity-ids])
        entities (map #(get-in world [:world/entities %]) entity-ids)
        ]
    entities))