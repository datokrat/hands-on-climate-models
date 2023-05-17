(ns state
  (:require scene item rect editor-state
            [clojure.math :as math])
  (:use util))

;; some planning

#_ "
Focus state:
-> Idle
-> Dragging
-> Input (either property or variable editor)

Timeline state: a number
-> if zero: drag-n-drop allowed (or should the t=0 scene be shown like 'ghosts' that can be manipulated?)

Scene editor state:
-> Idle
-> Selection
   -> Idle
   -> Dragging

Property editor state (parallel state during Selection):
-> Idle
-> Show
-> Input new variable name
-> Input expression or number

Variable editor state:
-> Idle
-> Input expression or number
"

(def clip-rect
  {:left 0 :right 800
   :top 100 :bottom 600})

(def clip-rect2
  (rect/ltrb 0 100 800 600))

(defn initial []
  (let [scene (scene/initial)]
    {:type :scene
     :data {:scene scene
            :editor (editor-state/initial scene nil)}
     :mouse {:x 0 :y 0}}))

(defn state-type [state]
  (:type state))

(defmulti get-scene state-type)

(defmulti update-scene (fn [state f] (state-type state)))

(defmulti set-scene (fn [state scene] (state-type state)))

(defn update-scene [state f]
  (set-scene state (f (get-scene state))))

(defmulti editor state-type)

(defmulti selection state-type)

(defmulti can-abort-transaction? state-type)

(defmulti abort-transaction state-type)

(defmulti deselect state-type)

(defmulti create-and-drag-item (fn [state _ _ _] (state-type state)))

(defmulti start-dragging (fn [state id] (state-type state)))

(defmulti on-key (fn [state key] (state-type state)))

(defmulti on-text (fn [state text] (state-type state)))

(defn get-item [state id]
  (-> state get-scene (scene/get-item id)))

(defn selected-item [state]
  (some->> state selection (get-item state)))

(defn items [state]
  (-> state get-scene scene/get-items))

(defmulti make-item (fn [type x y] type))

(defmethod make-item :sun [_ x y]
  (item/sun (item/value x) (item/value y) (item/value 100)))

(defmethod make-item :earth [_ x y]
  (item/earth (item/value x) (item/value y) (item/value 75)))

(defmethod make-item :arrow [_ x y]
  (item/arrow (item/value x) (item/value y) (item/value 25)))

(defmulti mouse-moved-to-scene-pos (fn [state x y] (state-type state)))

(defmulti mouse-released state-type)

(defn create-item [state item-type x y]
  (let [item (make-item item-type x y)
        [id scene] (-> state get-scene (scene/add-new-item item))]
    [id (set-scene state scene)]))

(defn create-and-drag-item [state tool x y]
  (let [[id state] (create-item state tool x y)]
    (start-dragging state id)))

(defn can-grab-tool? [state]
  (can-abort-transaction? state))

(defn grab-tool [state tool x y]
  (when (can-abort-transaction? state)
    (-> state
        abort-transaction
        (create-and-drag-item tool x y))))

(defn can-grab? [state]
  (can-abort-transaction? state))

(defn grab [state id]
  (when (can-grab? state)
    (-> state
        abort-transaction
        (start-dragging id))))

(defn move-mouse [state x y]
  (-> state
      (assoc :mouse {:x x :y y})
      (mouse-moved-to-scene-pos x y)))

(defn try-grab-slider [state]
  (when (can-abort-transaction? state)
    (assoc state
           :type :slider
           :data {:scene (get-scene state)
                  :editor (editor state)})))

;; scene

(defmethod get-scene :scene [state]
  (get-in state [:data :scene]))

(defmethod set-scene :scene [state scene]
  (assoc-in state [:data :scene] scene))

(defmethod selection :scene [state]
  nil)

(defmethod deselect :scene [state]
  state)

(defmethod can-abort-transaction? :scene [state]
  true)

(defmethod abort-transaction :scene [state]
  state)

(defmethod editor :scene [state]
  (get-in state [:data :editor]))

(defmethod start-dragging :scene [state id]
  (let [scene (get-scene state)]
    (assoc state
           :type :dragging
           :data {:scene scene
                  :dragging id
                  :editor (editor-state/initial scene id)})))

(defmethod mouse-moved-to-scene-pos :scene [state x y]
  state)

(defmethod mouse-released :scene [state]
  state)

(defmethod on-key :scene [state key]
  state)

(defmethod on-text :scene [state text]
  state)

;; selection

(defmethod get-scene :selection [state]
  (get-in state [:data :scene]))

(defmethod set-scene :selection [state scene]
  ;; TODO: What should happen if the selected item is removed?
  (assoc-in state [:data :scene] scene))

(defmethod selection :selection [state]
  (get-in state [:data :selection]))

(defmethod deselect :selection [state]
  (assoc state
         :type :scene
         :data {:scene (get-scene state)
                :editor (state/editor state)}))

(defmethod editor :selection [state]
  (get-in state [:data :editor]))

(defmethod can-abort-transaction? :selection [state]
  true)

(defmethod abort-transaction :selection [state]
  state)

(defmethod start-dragging :selection [state id]
  (let [scene (get-scene state)
        new-editor (if (= id (selection state))
                 (editor state)
                 (editor-state/initial scene id))]
    (assoc state
           :type :dragging
           :data {:scene scene
                  :dragging id
                  :editor new-editor})))

(defmethod mouse-moved-to-scene-pos :selection [state x y]
  state)

(defmethod mouse-released :selection [state]
  state)

(defmethod on-key :selection [state key]
  state)

;; dragging

(defmethod get-scene :dragging [state]
  (get-in state [:data :scene]))

(defmethod set-scene :dragging [state scene]
  ;; TODO: What should happen if the selected item is removed?
  (assoc-in state [:data :scene] scene))

(defmethod editor :dragging [state]
  (get-in state [:data :editor]))

(defmethod selection :dragging [state]
  (get-in state [:data :dragging]))

(defmethod selection :deselect [state]
  (assoc state
         :type :scene
         :data {:scene (get-scene state)
                :editor (editor state)}))

(defmethod can-abort-transaction? :dragging [state]
  true)

(defmethod abort-transaction :dragging [state]
  (let [scene (get-scene state)
        id (selection state)]
    (assoc state
           :type :selection
           :data {:scene scene
                  :selection id
                  :editor (editor state)})))

(defn clamp [x l u]
  (-> x (max l) (min u)))

(defn clamp-x [x]
  (clamp x (:left clip-rect) (:right clip-rect)))

(defn clamp-y [y]
  (clamp y (:top clip-rect) (:bottom clip-rect)))

(comment (defn move [item x y]
           (let [x (clamp x (:left clip-rect) (:right clip-rect))
                 y (clamp y (:top clip-rect) (:bottom clip-rect))]
             (item/move-to item x y))))

(defmethod mouse-moved-to-scene-pos :dragging [state x y]
  (if-not (-> state get-scene scene/is-at-start)
    state
    (let [dragged-id (selection state)]
      (set-scene state
                 (-> state state/get-scene
                    (scene/set-property-if-possible dragged-id :x (clamp-x x))
                    (scene/set-property-if-possible dragged-id :y (clamp-y y)))))))

(defmethod mouse-released :dragging [state]
  (abort-transaction state))

(defmethod on-key :dragging [state key]
  state)

(defmethod on-text :dragging [state text]
  state)

;; editing

(defn start-editing-property [state propkey]
  (let [scene (state/get-scene state)
        id (state/selection state)
        editor (state/editor state)]
    (assoc state
           :type :editing-property
           :data {:scene scene
                  :selection id
                  :editor (editor-state/edit-property scene id editor propkey)})))

(defn start-editing-variable [state name]
  (let [scene (state/get-scene state)
        id (state/selection state)
        editor (state/editor state)]
    (assoc state
           :type :editing-variable
           :data {:scene scene
                  :selection id
                  :editor (editor-state/edit-variable scene id editor name)})))

(defn toggle-variable [state name]
  (state/update-scene state (fn [scene] (scene/update-variable scene name variable/toggle))))

(defn start-creating-variable [state]
  (let [scene (state/get-scene state)
        id (state/selection state)
        editor (state/editor state)]
    (assoc state
           :type :creating-variable
           :data {:scene scene
                  :selection id
                  :editor (editor-state/create-variable scene id editor)})))

(defmethods get-scene [:editing-property, :editing-variable, :creating-variable] [state]
  (get-in state [:data :scene]))

(defmethods set-scene [:editing-property, :editing-variable, :creating-variable] [state value]
  (assoc-in state [:data :scene] value))

(defmethods selection [:editing-property, :editing-variable, :creating-variable] [state]
  (get-in state [:data :selection]))

(defmethods can-abort-transaction? [:editing-property, :editing-variable, :creating-variable] [state]
  false)

(defmethods mouse-moved-to-scene-pos [:editing-property, :editing-variable, :creating-variable] [state x y]
  state)

(defmethods mouse-released [:editing-property, :editing-variable, :creating-variable] [state]
  state)

(defmethods editor [:editing-property, :editing-variable, :creating-variable] [state]
  (get-in state [:data :editor]))

(defn finish-editing-property [state]
  (-> state
      (assoc :type :selection
             :data {:scene (state/get-scene state)
                    :selection (state/selection state)
                    :editor (editor-state/initial (state/get-scene state) (state/selection state))})))

(defn save-editing-property [state]
  (let [str (get-in state [:data :editor :value])
        propkey (get-in state [:data :editor :property])
        parsed (editor-state/parse-prop-expression str)
        id (state/selection state)]
    (-> state
        (update-in [:data :scene] #(scene/update-item % id (fn [item] (item/assoc-property item propkey parsed))))
        finish-editing-property)))

(defn save-editing-variable [state]
  (let [str (get-in state [:data :editor :value])
        [kind name] (get-in state [:data :editor :variable])]
    (-> state
        (update-in [:data :scene] #(scene/update-variable % name (fn [var] (variable/set-variable-line var kind str))))
        finish-editing-property)))

(defn confirm-new-variable-name [state]
  (let [editor (state/editor state)
        name (:value editor)]
    (-> state
        (assoc-in [:data :scene :variables name] (variable/make "0"))
        finish-editing-property)))

(defmethod on-key :editing-property [state k]
  (case k
    :backspace (update-in state [:data :editor] #(editor-state/backspace %))
    :enter (save-editing-property state)))

(defmethod on-key :editing-variable [state k]
  (case k
    :backspace (update-in state [:data :editor] #(editor-state/backspace %))
    :enter (save-editing-variable state)))

(defmethod on-key :creating-variable [state k]
  (case k
    :backspace (update-in state [:data :editor] #(editor-state/backspace %))
    :enter (confirm-new-variable-name state)))

(defmethods on-text [:editing-property, :editing-variable, :creating-variable] [state text]
  (update-in state [:data :editor] #(editor-state/text % text)))

;; slider

(defmethod get-scene :slider [state]
  (get-in state [:data :scene]))

(defmethod editor :slider [state]
  (get-in state [:data :editor]))

(defmethod selection :slider [state]
  nil)

(defmethod mouse-released :slider [state]
  (assoc state
         :type :scene
         :data {:scene (get-scene state)
                :editor (editor state)}))

(defmethod mouse-moved-to-scene-pos :slider [state x y]
  (update-in state [:data :scene] #(scene/set-time % (math/floor-div (- x 100) 50))))
