(ns editor-state
  (:require scene item))

;; parsing

(defmulti prop-expression item/prop-type)

(defmethod prop-expression :value [item]
  (str (:value item)))

(defmethod prop-expression :variable [item]
  (str "$" (:variable item)))

(defn parse-prop-expression [string]
  (if (clojure.string/starts-with? string "$")
    {:type :variable
     :variable (subs string 1)}
    {:type :value
     :value (Double. string)}))




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
  {:type :show})

(defn edit-property [scene id state propkey]
  {:type :property
   :property propkey
   :value ""})

(defn edit-variable [scene id state name]
  {:type :variable
   :variable name
   :value ""})

(defn create-variable [scene id state]
  {:type :create-variable
   :value ""})

(defn delete-last-char [str]
  (if (> (count str) 0)
    (subs str 0 (-> str count dec))
    str))

(defn backspace [editor-state]
  (update editor-state :value delete-last-char))

(defn text [editor-state text]
  (update editor-state :value #(str % text)))
