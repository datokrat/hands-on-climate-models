(ns scene)

(defn initial []
  {:items (sorted-map)
   :next-id 0})

(defn item-data
  [x y]
  {:x x :y y})

(defn add-new-item
  [state data]
  (let [id (:next-id state)]
    (-> state
        (update :next-id inc)
        (assoc-in [:items id] data))))

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
