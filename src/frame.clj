(ns frame)

(defn make [item-data variable-values]
  {:item-data item-data
   :variable-values variable-values})

(defn props [frame]
  (:item-data frame))

(defn variable-values [frame]
  (:variable-values frame))

(defn variable-value [frame name]
  (get-in frame [:variable-values name]))
