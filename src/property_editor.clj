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

(defn is-edit-click [i x y]
  (single-property-editor/input-contains x (+ layout/font-height (- y (single-y i)))))

(defn property-to-edit [state x y]
  (->> state
       state/selected-item
       item/properties
       (keep-indexed (fn [i propkey] (when (is-edit-click i x y) propkey)))
       first))

(defn on-press [state x y]
  (when (and (state/can-abort-transaction? state) (state/selection state))
    (println x y)
    (let [selection (state/selection state)
          property-editors (-> state state/editor :property-editors)
          edit-property (property-to-edit state x y)]
      (when edit-property
        (-> state
            state/abort-transaction
            (state/start-editing-property edit-property))))))

(defn draw-tabbed [canvas x y l text]
  (text/draw-text canvas
                  {:pos {:x x :y (line y l)}
                   :text (str text)}))

(defn draw-variable
  [canvas y name variable frame-data]
  (run! #(apply draw-tabbed canvas %)
        [[layout/tab1 y 0 (str name)]
         [layout/tab2 y 0 variable]
         [layout/tab2 y 1 frame-data]]))

(defn draw-variables
  [canvas variables varframe]
  (with-local-vars [y 100]
    (doseq [[name value] variables]
      (draw-variable canvas @y name value (get varframe name))
      (var-set y (+ @y 20)))))

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
    (case (:type editor)
      :show (draw-single-readonly canvas state i propkey)
      :property (if (= (:property editor) propkey)
                  (draw-single-editing-property canvas state i propkey)
                  (draw-single-readonly canvas state i propkey)))))

(defn draw-properties [canvas state]
  (let [properties (-> state state/selected-item item/properties)]
    (doseqi i [propkey properties]
            (draw-single canvas state i propkey)))
  (comment (let [property-editors (:property-editors editor-state)]
     (doseqi i [propkey (-> scene (scene/get-item id) item/properties)]
             (single-property-editor/draw canvas
                                          {:scene scene
                                           :id id
                                           :propkey propkey}
                                          (get property-editors propkey)
                                          (single-y i))))))

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
    (draw-variables canvas variables varframe)))
