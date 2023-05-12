(ns state
  (:require scene item))

(def clip-rect
  {:left 0 :right 800
   :top 100 :bottom 600})

(defn initial []
  {:scene (scene/initial)
   :dragging nil
   :selection nil
   :mouse {:x 0 :y 0}})

(defn get-item [state id]
  (-> state :scene (scene/get-item id)))

(defn dragged-item [state]
  (get-item state (:dragging state)))

(defn selection [state]
  (get-item state (:selection state)))

(defn items [state]
  (-> state :scene :items))

(defn new-id [state]
  (let [state' (update state :next-id inc)]
    [(:next-id state) state']))

(defn item [id x y]
  {:id id :x x :y y})

(defn clamp [x l u]
  (-> x (max l) (min u)))

(defn move [item x y]
  (let [x (clamp x (:left clip-rect) (:right clip-rect))
        y (clamp y (:top clip-rect) (:bottom clip-rect))]
    (item/move-to item x y)))

(defn select [state id]
  (assoc state :selection id))

(defmulti make-item (fn [type x y] type))

(defmethod make-item :sun [_ x y]
  (item/sun x y 100))

(defmethod make-item :earth [_ x y]
  (item/earth x y 75))

(defn create-item [state item-type x y]
  (let [item (make-item item-type x y)
        [id scene] (-> state :scene (scene/add-new-item item))]
    [id (assoc state :scene scene)]))

(defn create-and-drag-item [state item-type x y]
  (let [[id state] (create-item state item-type x y)]
    (assoc state :dragging id)))

(defn drag-to [state x y]
  (if-let [dragged-id (:dragging state)]
    (let [update-item #(move % x y)
          update-scene #(scene/update-item % dragged-id update-item)]
      (update state :scene update-scene))
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
