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
   :value "Hi"})

(defn delete-last-char [str]
  (if (> (count str) 0)
    (subs str 0 (-> str count dec))
    str))

(defn backspace [editor-state]
  (if (= (:type editor-state) :property)
    (update editor-state :value delete-last-char)
    editor-state))

(defn text [editor-state text]
  (if (= (:type editor-state) :property)
    (update editor-state :value #(str % text))))
