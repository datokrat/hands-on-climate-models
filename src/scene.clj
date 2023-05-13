(ns scene)

(defn initial []
  {:items (sorted-map)
   :variables (sorted-map)
   :next-id 0
   :time 0})

(defn get-items [scene]
  (:items scene))

(defn get-frame-at [scene t]
  (->> scene get-items
      (map (fn [[id item]]
             [id {:x (+ (* 5 t) (item/get-x item))
                  :y (item/get-y item)}]))
      (into (sorted-map))))

(defn get-frame [scene]
  (get-frame-at scene (:time scene)))

(defn get-item [scene id]
  (-> scene get-items (get id)))

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

(defn add-new-variable
  [scene name variable]
  (-> scene
      (assoc-in [:variables name] variable)))

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

(defn get-time [scene]
  (:time scene))

(defn set-time [scene t]
  (assoc scene :time t))

(defn is-at-start [scene]
  (-> scene get-time (= 0)))
