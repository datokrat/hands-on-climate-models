(ns property-editor
  (:require state item rect text frame layout single-property-editor)
  (:use util))

(def clip-rect
  (rect/ltrb 800 0 1200 600))

(def pos1 {:x 800 :y 20})
(def pos2 {:x 800 :y 60})
(def var-pos {:x 800 :y 100})

(def rect1
  (rect/ltrb 800 0 1200 20))

(def rect2
  (rect/ltrb 800 40 1200 60))


(defn line [y n]
  (+ y (* n 20)))

(defn under-pos
  [state x y]
  (when (.contains clip-rect x y)
    true))

(defn single-y [i]
  (+ 20 (* i (single-property-editor/height))))

(def variable-height (* 3 layout/font-height))

(defn single-var-y [i]
  (+ 250 (* i variable-height)))

(defn var-line-y [i kind]
  (+ (single-var-y i)
     (case kind
       :dependent 0
       :initial 0
       :change layout/font-height)))

(defn on-press-single [state propkey i editor-state x y]
  (when-let [{:keys [property-editor]}
             (single-property-editor/on-press
              {:scene (state/get-scene state)
               :id (state/selection state)
               :propkey propkey}
              editor-state
              x (- y (single-y i)))]
    (-> state
        state/abort-transaction
        state/start-editing (cond-> state
                              property-editor (assoc-in [:data :editor :property-editors propkey] property-editor)))))

(defn is-edit-click [field-y x y]
  (single-property-editor/input-contains x (+ layout/font-height (- y field-y))))

(defn is-name-click [field-y x y]
  (and (<= x layout/tab2)
       (<= 0 (+ layout/font-height (- y field-y)) layout/font-height)))

(defn property-to-edit [state x y]
  (->> state
       state/selected-item
       item/properties
       (keep-indexed (fn [i propkey] (when (is-edit-click (single-y i) x y) propkey)))
       first))

(defn variable-to-edit [state x y]
  (->> state
       state/get-scene
       scene/get-variables
       (map-indexed (fn [i [name variable]] [i name variable]))
       (mapcat (fn [[i name variable]]
                 (case (:type variable)
                   :dependent [[i :dependent name]]
                   :differential [[i :initial name] [i :change name]])))
       (keep (fn [[i kind name]] (when (is-edit-click (var-line-y i kind) x y) [kind name])))
       first))

(defn should-create-variable? [state x y]
  (let [button-y (-> state state/get-scene scene/get-variables count single-var-y)]
    (and (<= x layout/tab2)
         (<= button-y y (+ button-y layout/font-height)))))

(defn variable-to-toggle [state x y]
  (->> state
       state/get-scene
       scene/get-variables
       first
       (keep-indexed (fn [i name] (when (is-name-click (single-var-y i) x y) name)))
       first))

(defn on-press [state x y]
  (when (and (state/can-abort-transaction? state) (state/selection state))
    (let [selection (state/selection state)]
      (or
       (when-let [edit-property (property-to-edit state x y)]
         (-> state
             state/abort-transaction
             (state/start-editing-property edit-property)))
       (when-let [edit-variable (variable-to-edit state x y)]
         (-> state
             state/abort-transaction
             (state/start-editing-variable edit-variable)))
       (when-let [toggle-variable (variable-to-toggle state x y)]
         (-> state
             state/abort-transaction
             (state/toggle-variable toggle-variable)))
       (when (should-create-variable? state x y)
         (-> state
             state/abort-transaction
             state/start-creating-variable))))))

(defn draw-tabbed [canvas x y l text]
  (text/draw-text canvas
                  {:pos {:x x :y (line y l)}
                   :text (str text)}))

(defn editing-variable? [state name]
  (let [editor (state/editor state)]
    (and (= (:type editor) :variable)
         (= (get-in editor [:variable 1]) name))))

(defn displayed-dependent-value [state name]
  (let [editor (state/editor state)
        variable (-> state state/get-scene (scene/get-variable name))]
    (if (and (= (:type editor) :variable)
             (= (:variable editor) [:dependent name]))
      (:value editor)
      (:value variable))))

(defn displayed-initial-value [state name]
  (let [editor (state/editor state)
        variable (-> state state/get-scene (scene/get-variable name))]
    (if (and (= (:type editor) :variable)
             (= (:variable editor) [:initial name]))
      (:value editor)
      (:initial variable))))

(defn displayed-change-value [state name]
  (let [editor (state/editor state)
        variable (-> state state/get-scene (scene/get-variable name))]
    (if (and (= (:type editor) :variable)
             (= (:variable editor) [:change name]))
      (:value editor)
      (:change variable))))

(defn draw-variable-readonly-dependent
  [canvas state y name variable frame-data]
  (run! #(apply draw-tabbed canvas %)
        [[layout/tab1 y 0 (str name)]
         [layout/tab2 y 0 (displayed-dependent-value state name)]])
  (when-not (editing-variable? state name)
    (draw-tabbed canvas layout/tab3 y 0 (str " => " frame-data))))

(defn draw-variable-readonly-differential
  [canvas state y name variable frame-data]
  (run! #(apply draw-tabbed canvas %)
        [[layout/tab1 y 0 (str name " (t=0)")]
         [layout/tab2 y 0 (displayed-initial-value state name)]
         [layout/tab1 y 1 (str "d " name)]
         [layout/tab2 y 1 (displayed-change-value state name)]])
  (when-not (editing-variable? state name)
    (draw-tabbed canvas layout/tab3 y 0 (str " => " frame-data))))

(defn draw-variable-readonly
  [canvas state y name variable frame-data]
  (case (:type variable)
    :dependent (draw-variable-readonly-dependent canvas state y name variable frame-data)
    :differential (draw-variable-readonly-differential canvas state y name variable frame-data)))

(defn draw-variable-editing
  [canvas y name variable frame-data]
  (run! #(apply draw-tabbed canvas %)
        [[layout/tab1 y 0 "Editing"]]))

(defn draw-variable
  [canvas state y name variable frame-data]
  (draw-variable-readonly canvas state y name variable frame-data))

(defn draw-new-var-button [canvas state i]
  (let [y (single-var-y i)
        editor (state/editor state)]
    (if (= (:type editor) :create-variable)
      (run! #(apply draw-tabbed canvas %)
            [[layout/tab1 y 0 (:value editor)]])
      (run! #(apply draw-tabbed canvas %)
            [[layout/tab1 y 0 "new"]]))))

(defn draw-variables
  [canvas state variables varframe]
  (doseqi i [[name value] variables]
          (draw-variable canvas state (single-var-y i) name value (get varframe name)))
  (draw-new-var-button canvas state (count variables)))

(defn draw-single-readonly [canvas state i propkey]
  (let [id (state/selection state)
        item (state/selected-item state)
        prop (-> item (item/get-property propkey))
        propframe (-> state state/get-scene scene/get-frame frame/props)
        y (single-y i)]
    (run! #(apply draw-tabbed canvas %)
          [[layout/tab1 y 0 (name propkey)]
           [layout/tab2 y 0 (single-property-editor/prop-expression prop)]
           [layout/tab3 y 0 (str "=> " (get-in propframe [id propkey]))]])))

(defn draw-single-editing-property [canvas state i propkey]
  (let [value (-> state state/editor :value)
        y (single-y i)]
    (run! #(apply draw-tabbed canvas %)
          [[layout/tab1 y 0 (name propkey)]
           [layout/tab2 y 0 value]])))

(defn draw-single [canvas state i propkey]
  (let [editor (state/editor state)]
    (if-not (and (= (:type editor) :property)
                 (= (:property editor) propkey))
      (draw-single-readonly canvas state i propkey)
      (draw-single-editing-property canvas state i propkey))))

(defn draw-properties [canvas state]
  (let [properties (-> state state/selected-item item/properties)]
    (doseqi i [propkey properties]
            (draw-single canvas state i propkey))))

(defn draw-property-editor
  [canvas state]
  (let [scene (state/get-scene state)
        frame (scene/get-frame scene)
        variables (scene/get-variables scene)
        propframe (frame/props frame)
        varframe (frame/variable-values frame)
        y 20]
    (when (state/selection state)
      (draw-properties canvas state))
    (draw-variables canvas state variables varframe)))
