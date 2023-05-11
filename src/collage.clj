(ns collage
  (:require
   state
   property-editor)
  (:import
   [io.github.humbleui.types Rect]))

(def sprite-rect
  (Rect/makeXYWH -100 -100 200 200))

(defn item-rect
  [state id]
  (let [{:keys [x y]} (get-in state [:items id])]
    (.offset sprite-rect x y)))

(defn tool-rect []
  (-> sprite-rect
      (.offset 100 100)
      (.scale 0.5)))

(defn contains
  [rect x y]
  (.contains rect x y))

(defn item-under-pos
  [state x y]
  (->> state
      state/items
      reverse
      keys
      (filter #(contains (item-rect state %) x y))
      first))

(defn tool-under-pos
  [state x y]
  (when (contains (tool-rect) x y)
    {}))

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
  [under-pos]
  (if-let [[type object] under-pos]
    (= type :tool)
    false))

(defn sprites
  [state x y]
  (let [under-pos (object-under-pos state x y)]
     (concat
      (->> state
           :items
           (map second)
           (map #(hash-map :type :item
                           :item %
                           :hovered? (item-hovered? under-pos (:id %)))))
      (list {:type :tool
        :hovered? (tool-hovered? under-pos)})
      (property-editor/sprites state))))
