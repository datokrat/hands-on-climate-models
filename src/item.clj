(ns item
  (:require rect)
  (:use util))

(defn type [item]
  (first item))

(defn move-to [item x y]
  (update item 1 #(into % {:x x :y y})))

(defn get-x [item]
  (-> item second :x))

(defn get-y [item]
  (-> item second :y))

;; interface

(defmulti in-range? (fn [item frame-data point] (type item)))

(defmulti bounding-box (fn [item frame-data] (type item)))

(defmulti properties-for-type identity)

(defn properties [item]
  (-> item type properties-for-type))

(defmulti assoc-property (fn [item key value] (type item)))

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

(comment
  (assoc-property (sun 0 0 0) :x 1)
  (assoc [1 2] 0 3))
