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

;; ideas: derivatives, expressions with data flow engine
