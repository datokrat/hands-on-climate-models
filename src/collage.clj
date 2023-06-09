(ns collage
  (:require
   state rect
   property-editor)
  (:import
   [io.github.humbleui.types Rect]))

(def tools
  [:sun :earth :arrow-right :arrow-left])

(defn item-rect
  [state id]
  (-> state
      (state/get-item id)
      (item/bounding-box)))

(defn tool-offset [tool]
  (case tool
    :sun [50 50]
    :earth [150 50]
    :arrow-right [250 50]
    :arrow-left [350 50]))

(defn tool-rect-scaled [tool scale]
  (case tool
    :arrow-right (rect/ltrb 200 25 300 75)
    :arrow-left (rect/ltrb 300 25 400 75)
    (as-> (rect/centered-square 100) $
      (rect/scale $ scale)
      (apply rect/offset $ (tool-offset tool)))))

(defn tool-rect [tool]
  (tool-rect-scaled tool 1))

(defn item-under-pos
  [state x y]
  (let [propframe (-> state state/get-scene scene/get-frame frame/props)]
    (->> propframe reverse keys
         (filter #(-> state (state/get-item %)
                      (item/in-range? (get propframe %) {:x x :y y})))
         first)))

(defn tool-under-pos
  [state x y]
  (->> tools reverse
       (filter #(-> % tool-rect (.contains x y)))
       first))

(defn object-under-pos
  [state x y]
  (or
   (when (property-editor/under-pos state x y)
     [:property-editor nil])
   (when-let [tool (tool-under-pos state x y)]
     [:tool tool])
   (when-let [item (item-under-pos state x y)]
     [:item item])))

(defn item-hovered?
  [under-pos id]
  (if-let [[type object] under-pos]
    (and
     (= type :item)
     (= object id))
    false))

(defn tool-hovered?
  [under-pos tool]
  (if-let [[type object] under-pos]
    (and (= type :tool)
     (= object tool))
    false))

(defn sprites
  [state x y]
  (let [under-pos (object-under-pos state x y)]
     (concat
      (->> state
           state/items
           (map #(hash-map :type :item
                           :id (first %)
                           :item (second %)
                           :hovered? (item-hovered? under-pos (first %)))))
      (list {:type :tool
             :tool :sun
             :hovered? (tool-hovered? under-pos :sun)}
            {:type :tool
             :tool :earth
             :hovered? (tool-hovered? under-pos :earth)}
            {:type :tool
             :tool :arrow-right
             :hovered? (tool-hovered? under-pos :arrow-right)}
            {:type :tool
             :tool :arrow-left
             :hovered? (tool-hovered? under-pos :arrow-left)}))))
