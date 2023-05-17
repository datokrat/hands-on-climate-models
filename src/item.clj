(ns item
  (:require rect)
  (:use util))

(defn type [item]
  (first item))

(comment (defn move-to [item x y]
   (update item 1 #(into % {:x x :y y}))))

(defn get-x [item]
  (-> item second :x))

(defn get-y [item]
  (-> item second :y))

(defn value [value]
  {:type :value
   :value value})

(defn prop-type [property]
  (:type property))

;; interface

(defmulti in-range? (fn [item frame-data point] (type item)))

(defmulti bounding-box (fn [item frame-data] (type item)))

(defmulti properties-for-type identity)

(defn properties [item]
  (-> item type properties-for-type))

(defmulti get-property (fn [item key] (type item)))

(defmulti assoc-property (fn [item key value] (type item)))

;; arrow

(defn arrow [x y thickness]
  [:arrow {:x x :y y :thickness thickness}])

(defmethod bounding-box :arrow [item frame-data]
  (let [t (:thickness frame-data)]
    (println "bounding-box" frame-data)
    (rect/offset (rect/ltrb -100 (- t) 100 t) (:x frame-data) (:y frame-data))))

(defmethod in-range? :arrow [item frame-data point]
  (.contains (bounding-box item frame-data) (:x point) (:y point)))

(defmethod properties-for-type :arrow [_]
  [:x :y :thickness])

(defmethod get-property :arrow [item k]
  (println "get-property" item k)
  (get-in item [1 k]))

(defmethod assoc-property :arrow [item k value]
  (assoc-in item [1 k] value))

;; sun, earth

(def celestials [:sun, :earth])

(defn sun [x y r]
  [:sun {:x x :y y :radius r}])

(defn earth [x y r]
  [:earth {:x x :y y :radius r}])

(def planet-rect
  (rect/ltrb -100 -100 100 100))

(defn radius [planet]
  (get-in planet [1 :radius]))

(defn square-distance [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (+ (* dx dx)
       (* dy dy))))

(defn item->point [item]
  {:x (get-x item)
   :y (get-y item)})

(defmethods in-range? celestials [item point frame-data]
  (let [sqdist (square-distance frame-data point)
        item-radius (:radius frame-data)]
    (<= sqdist (* item-radius item-radius))))

(defmethods bounding-box celestials [item frame-data]
  (let [x (:x frame-data)
        y (:y frame-data)
        radius (:radius frame-data)]
    (-> radius (* 2) rect/centered-square (rect/offset x y))))

(defmethods properties-for-type celestials [_]
  [:x :y :radius])

(defmethods assoc-property celestials
  [item key value]
  (assoc-in item [1 key] value))

(defmethods get-property celestials
  [item key]
  (get-in item [1 key]))

(comment
  (assoc-property (sun 0 0 0) :x 1)
  (assoc [1 2] 0 3))
