(ns property-editor
  (:require state item rect))

(def clip-rect
  (rect/ltrb 800 0 1000 600))

(def pos1 {:x 800 :y 20})
(def pos2 {:x 800 :y 60})

(def rect1
  (Rect/makeLTRB 800 0 1000 20))

(def rect2
  (Rect/makeLTRB 800 40 1000 60))

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
    (list
     {:type :text
      :pos pos1
      :text (str "x: " (-> state state/selected-item item/get-x))}
     {:type :text
      :pos pos2
      :text (str "y: " (-> state state/selected-item item/get-y))})))
