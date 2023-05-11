(ns item)

(defn sun [x y r]
  [:sun {:x x :y y :radius r}])

(defn radius [sun-item]
  (get-in sun-item [1 :radius]))

(defn type [item]
  (first item))

(defn move-to [item x y]
  (update item 1 #(into % {:x x :y y})))

;; interface

(defmulti in-range? (fn [item point] (type item)))

(defmulti properties-for-type identity)

(defn properties [item]
  (-> item type properties-for-type))

(defmulti assoc-property (fn [item key value] (type item)))

(defn square-distance [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (+ (* dx dx)
       (* dy dy))))

;; sun

(defmethod in-range? :sun in-range-of-sun? [item point]
  (let [sqdist (square-distance item point)
        item-radius (radius item)]
    (<= sqdist (* item-radius item-radius))))

(defmethod properties-for-type :sun sun-properties [_]
  [:x :y :radius])

(defmethod assoc-property :sun assoc-sun-property
  [item key value]
  (assoc-in item [1 key] value))

(comment
  (assoc-property (sun 0 0 0) :x 1)
  (assoc [1 2] 0 3))
