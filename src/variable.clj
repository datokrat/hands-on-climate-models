(ns variable
  (:require [clojure.math :as math]))

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
    :dependent (make-differential "0" "0")
    :differential (make "0")))

(defn parse-number [str]
  (when (re-matches #"\d+(.\d+)?" str)
    (let [value (Double. str)]
      (fn [context-fn] value))))

(defn parse-formula [str]
  (when-let [matches (re-matches #"\$(\w+)\^4" str)]
    (let [name (second matches)]
      (fn [context-fn] (clojure.math/pow (context-fn name) 4.0)))))

(defn parse-formula2 [str]
  (when-let [matches (re-matches #"\$(\w+) - \$(\w+)" str)]
    (let [name1 (second matches)
          name2 (nth matches 2)]
      (fn [context-fn] (- (context-fn name1) (context-fn name2))))))


(defn parse-variable-line [str]
  (or (parse-number str)
      (parse-formula str)
      (parse-formula2 str)))

;; ideas: derivatives, expressions with data flow engine
