(ns scene
  (:require item frame variable))

(defn initial []
  {:items (sorted-map)
   :variables (sorted-map)
   :next-id 0
   :time 0})

(defn get-items [scene]
  (:items scene))

(defn get-variables [scene]
  (:variables scene))

(defn eval-initial-variable [name variables]
  (let [variable (get variables name)]
    (case (:type variable)
      :dependent ((variable/parse-variable-line (:value variable))
                  (fn [name] (eval-initial-variable name variables)))
      :differential (Double. (:initial variable)))))

(defn eval-consecutive-variable [last-varframe name variables]
  (let [variable (get variables name)
        last-value (get last-varframe name)]
    (case (:type variable)
      :dependent ((variable/parse-variable-line (:value variable))
                  (fn [name] (eval-consecutive-variable last-varframe name variables)))
      :differential (+ last-value
                       ((variable/parse-variable-line (:change variable))
                        (fn [name] (get last-varframe name)))))))

(defn get-initial-varframe [scene]
  (->> scene get-variables
         (map (fn [[name variable]]
                [name (eval-initial-variable name (get-variables scene))]))
         (into (sorted-map))))

(defn get-next-varframe [scene frame]
  (->> scene get-variables
       (map (fn [[name variable]]
              [name (eval-consecutive-variable frame  name (get-variables scene))]))
       (into (sorted-map))))

(defn get-varframe-at [scene t]
  (-> #(get-next-varframe scene %)
      (iterate (get-initial-varframe scene))
      (nth t)))

(defn eval-prop [prop varframe]
  (case (:type prop)
    :value (:value prop)
    :variable (get varframe (:variable prop))))

(defn get-propframe-at [scene t varframe]
  (->> scene get-items
       (map (fn [[id item]]
              [id {:x (eval-prop (item/get-x item) varframe)
                   :y (eval-prop (item/get-y item) varframe)
                   :radius (eval-prop (item/radius item) varframe)}]))
       (into (sorted-map))))

(defn get-frame-at [scene t]
  (let [varframe (get-varframe-at scene t)
        propframe (get-propframe-at scene t varframe)]
    (frame/make propframe varframe)))

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

(defn get-variable
  [scene name]
  (get-in scene [:variables name]))

(defn set-variable
  [scene name variable]
  (-> scene
      (assoc-in [:variables name] variable)))

(defn update-variable
  [scene name f]
  (set-variable scene name (f (get-variable scene name))))

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

(defn assign-variable-if-possible [variable value]
  (case (:type variable)
    :dependent (variable/make value)
    :differential (variable/make-differential value (:change variable))))

(defn set-property-if-possible [scene item-id propkey value]
  (let [item (-> scene (scene/get-item item-id))
        prop (item/get-property item propkey)]
    (case (:type prop)
      :value (scene/update-item
              scene item-id
              #(item/assoc-property % propkey (item/value value)))
      :variable (scene/update-variable
                 scene (:variable prop)
                 #(assign-variable-if-possible % value)))))
