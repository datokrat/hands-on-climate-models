(ns variable)

(defn make [value]
  {:type :dependent
   :value value})

(defn make-differential [initial change]
  {:type :differential
   :initial initial
   :change change})

(defn set [variable value]
  value)

(defn get [variable]
  variable)

(defn set-variable-line [variable kind value]
  (case kind
    :dependent (assoc variable :value value)
    :initial (assoc variable :initial value)
    :change (assoc variable :change value)))

(defn toggle [variable]
  (case (:type variable)
    :dependent (make-differential 0.0 0.0)
    :differential (make 0.0)))

;; ideas: derivatives, expressions with data flow engine
