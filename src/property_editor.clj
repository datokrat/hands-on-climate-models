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
    (cond-> state
      property-editor (assoc-in [:data :editor :property-editors propkey] property-editor))))

(defn on-press [state x y]
  (when (state/selection state)
    (let [property-editors (-> state state/editor :property-editors)]
      (or (some identity
                 (map-indexed (fn [i propkey] (on-press-single
                                               state
                                               propkey i
                                               (get property-editors propkey)
                                               x y))
                              (-> state state/selected-item item/properties)))
          state))))

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

(defn draw-properties [canvas scene id editor-state]
  (let [property-editors (:property-editors editor-state)]
    (doseqi i [propkey (-> scene (scene/get-item id) item/properties)]
            (single-property-editor/draw canvas
                                         {:scene scene
                                          :id id
                                          :propkey propkey}
                                         (get property-editors propkey)
                                         (single-y i)))))

(defn draw-property-editor
  [canvas state]
  (let [scene (state/get-scene state)
        frame (scene/get-frame scene)
        variables (scene/get-variables scene)
        propframe (frame/props frame)
        varframe (frame/variable-values frame)
        y 20]
    (when (state/selection state)
      (draw-properties canvas scene (state/selection state) (state/editor state)))
    (draw-variables canvas variables varframe)))
