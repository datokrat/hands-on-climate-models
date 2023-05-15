(ns editor-state
  (:require scene item))

(defn initial-single-state []
  {:editing :nothing}) ; :editing can be :nothing, :expression or :variable

(defn editing-single-state [string]
  {:editing :expression
   :value string})

(defn edit-mode [state]
  (:editing state))

(defn initial-property-editors [scene id]
  (let [propkeys (-> scene (scene/get-item id) item/properties)
        kvs (map #(vector % (initial-single-state)) propkeys)]
    (into {} kvs)))

(defn initial [scene id]
  {:property-editors (initial-property-editors scene id)})
