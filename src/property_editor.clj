(ns property-editor
  (:require state item rect text frame))

(def clip-rect
  (rect/ltrb 800 0 1000 600))

(def pos1 {:x 800 :y 20})
(def pos2 {:x 800 :y 60})
(def var-pos {:x 800 :y 100})

(def rect1
  (rect/ltrb 800 0 1000 20))

(def rect2
  (rect/ltrb 800 40 1000 60))

(defn under-pos
  [state x y]
  (when (.contains clip-rect x y)
    true))

(comment (defn on-press
   [state x y]
   (or
    (when (.contains rect1 x y)
      :x)
    (when (.contains rect2 x y)
      :y))))

(defn sprites
  [state]
  (when (state/selection state)
    (list #_ #_
     {:type :text
      :pos pos1
      :text (str "x: " (-> state state/selected-item item/get-x))}
     {:type :text
      :pos pos2
      :text (str "y: " (-> state state/selected-item item/get-y))})))

(defn draw-variable
  [canvas y name variable frame-data]
  (text/draw-text
       canvas
       {:pos {:x 800 :y y}
        :text (str name ": " variable ", " frame-data)}))

(defn draw-variables
  [canvas variables varframe]
  (with-local-vars [y 100]
    (doseq [[name value] variables]
      (draw-variable canvas @y name value (get varframe name))
      (var-set y (+ @y 20)))))

(defn draw-property-editor
  [canvas state]
  (let [scene (state/get-scene state)
        frame (scene/get-frame scene)
        variables (scene/get-variables scene)
        propframe (frame/props frame)
        varframe (frame/variable-values frame)]
    (when (state/selection state)
      (text/draw-text
       canvas
       {:pos pos1
        :text (str "x: " (get-in propframe [(state/selection state) :x]))})
      (text/draw-text
       canvas
       {:pos pos2
        :text (str "y: " (get-in propframe [(state/selection state) :y]))}))
    (draw-variables canvas variables varframe)))
