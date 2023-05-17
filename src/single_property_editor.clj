(ns single-property-editor
  (:require layout text scene frame editor-state)
  (:use util))

;; layout

(defn line [y n]
  (+ y (* n 20)))

(defn height []
  (* 3 layout/font-height))

;; drawing

(defn draw-tabbed [canvas x y l text]
  (text/draw-text canvas
                  {:pos {:x x :y (+ (line y l) layout/font-height)}
                   :text (str text)}))

(defmulti draw (fn [canvas {:keys [scene id propkey]} state y] (editor-state/edit-mode state)))

(defn prop-desc [item]
  (case (item/type item)
    :value "expression"
    :variable "variable"))

(defmethod draw :nothing [canvas {:keys [scene id propkey]} state y]
  (let [item (-> scene (scene/get-item id))
        prop (-> item (item/get-property propkey))
        propframe (-> scene scene/get-frame frame/props)]
    (run! #(apply draw-tabbed canvas %)
          [[layout/tab1 y 0 (name propkey)]
           [layout/tab2 y 0 (editor-state/prop-expression prop)]
           [layout/tab3 y 0 (str "=> " (get-in propframe [id propkey]))]])))

(defmethod draw :expression [canvas {:keys [scene id propkey]} state y]
  (let [value (:value state)]
    (run! #(apply draw-tabbed canvas %)
          [[layout/tab1 y 0 (name propkey)]
           [layout/tab2 y 0 value]])))

;; input

(defmulti on-press (fn [context state x y can-focus] (editor-state/edit-mode state)))

(defn input-contains [x y]
  (and
   (<= layout/tab2 x layout/tab3)
   (<= 0 y layout/font-height)))

(defmethod on-press :nothing [{:keys [scene id propkey]} state x y can-focus]
  (when (and can-focus (input-contains x y))
    {:property-editor (editor-state/editing-single-state "Hi")}))
