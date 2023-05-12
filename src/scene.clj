(ns scene)

(defn initial []
  {:items (sorted-map)
   :next-id 0})

(defn get-item [scene id]
  (get-in scene [:items id]))

(defn item
  [x y]
  {:x x :y y})

(defn add-new-item
  [scene data]
  (let [id (:next-id scene)]
    [id
     (-> scene
         (update :next-id inc)
         (assoc-in [:items id] data))]))

(defn change-item
  [state id data]
  (assoc-in state [:items id] data))

(defn update-item
  [state id f]
  (update-in state [:items id] f))

(defn item
  [id data]
  {:id id :data data})

(defn move-item
  [data x y]
  (into data {:x x :y y}))
