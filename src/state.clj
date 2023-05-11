(ns state)

(def clip-rect
  {:left 0 :right 800
   :bottom 100 :top 600})

(defn initial []
  {:items (sorted-map)
   :dragging nil
   :next-id 0
   :mouse {:x 0 :y 0}})

(defn dragged-item [state]
  (get-in state [:items (:dragging state)]))

(defn selection [state]
  (let [id (:selection state)]
    (get-in state [:items id])))

(defn items [state]
  (:items state))

(defn new-id [state]
  (let [state' (update state :next-id inc)]
    [(:next-id state) state']))

(defn item [id x y]
  {:id id :x x :y y})

(defn clamp [x l u]
  (-> x (max l) (min u)))

(defn move [item x y]
  (-> item (assoc :x (clamp x (:left clip-rect) (:right clip-rect))) (assoc :y (clamp y (:bottom clip-rect) (:top clip-rect)))))

(defn select [state id]
  (assoc state :selection id))

(defn create-item [state x y]
  (let [[id state] (new-id state)
        new-item (item id x y)
        state (assoc-in state [:items id] new-item)]
    [id state]))

(defn create-and-drag-item [state x y]
  (let [[id state] (create-item state x y)
        state (assoc state :dragging id)]
    state))

(defn drag-to [state x y]
  (if (:dragging state)
    (update-in state [:items (:dragging state)] #(move % x y))
    state))

(defn move-mouse [state x y]
  (-> state
      (drag-to x y)
      (assoc-in [:mouse :x] x)
      (assoc-in [:mouse :y] y)))

(defn grab [state id]
  (assoc state :dragging id))

(defn release [state]
  (-> state
      (assoc :selection (:dragging state))
      (assoc :dragging nil)))
