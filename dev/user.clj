(ns user
  (:require
   state
   collage
   [nrepl.cmdline :as nrepl])
  (:import
   [io.github.humbleui.types Rect]
   [io.github.humbleui.jwm App EventMouseMove EventMouseButton]
   [io.github.humbleui.skija Image FontMgr Font FontStyle Paint]
   [io.github.humbleui.jwm.skija EventFrameSkija LayerGLSkija]
   [java.util.function Consumer]))

(defn -main
  [& args]
  (apply nrepl/-main args))

(defn start []
  (future (App/start (constantly nil))))

(defmacro throw->data [& forms]
  `(try {:success true :value (do ~@forms)}
        (catch Throwable t# {:success false :value t#})))

(defn data->throw [data]
  (if (:success data)
    (:value data)
    (throw (:value data))))

(defmacro doui-async [& forms]
  `(let [p# (promise)]
     (App/runOnUIThread #(deliver p# (throw->data ~@forms)))
     p#))

(defmacro doui [& forms]
  `(let [r# (deref (doui-async ~@forms))]
     (data->throw r#)))

(defn create []
  (def w (doui (App/makeWindow))))

(defn consumer [accept-fn]
  (reify Consumer
    (accept [_ x] (accept-fn x))))

(defmacro case-instance
  [e & clauses]
  (condp instance? ~e ~@clauses))

(defn str->sprite
  [str]
  (-> str
      (java.nio.file.Path/of (into-array java.lang.String []))
      java.nio.file.Files/readAllBytes
      Image/makeFromEncoded))

(def sprite-path-str
  "/home/paul/Bilder/isosmile.png")

(def sprite-path
  (java.nio.file.Path/of sprite-path-str (into-array java.lang.String [])))

(def sprite-bytes
  (java.nio.file.Files/readAllBytes sprite-path))

(def sprite
  (str->sprite "./img/sun.png"))

(def sprite-rect
  (Rect/makeXYWH -100 -100 200 200))

(def mouse-x 0)
(def mouse-y 0)

(def state
  {:items (sorted-map 0 {:id 0 :x 0 :y 0})
   :dragging 0
   :next-id 1
   :mouse {:x 0 :y 0}})

(defn request-frame []
  (doui (.requestFrame w)))

(defn draw-toolbar
  [canvas hovered?]
  (.drawImageRect canvas sprite (-> (Rect/makeXYWH 0 0 100 100)
                                    (.scale (if hovered? 1.2 1)))))

(defn draw-item
  [canvas item hovered?]
  (.drawImageRect canvas sprite (-> sprite-rect
                                    (.offset (:x item) (:y item)))))

(def font-mgr (FontMgr/getDefault))

(def typeface (.matchFamilyStyle font-mgr "Liberation Sans" FontStyle/NORMAL))

(def font (Font. typeface (float 20)))

(defn draw-text
  [canvas object]
  (with-open [fill (Paint.)]
    (println (:text object) (get-in [:pos :x] object) (get-in [:pos :y] object) font fill)
    (.drawString canvas
                 (:text object)
                 (get-in object [:pos :x])
                 (get-in object [:pos :y])
                 font
                 fill)))

(defn draw-object
  [canvas object]
  (case (:type object)
    :tool (draw-toolbar canvas (:hovered? object))
    :item (draw-item canvas (:item object) (:hovered? object))
    :text (draw-text canvas object)))

(defn on-paint
  [event]
  (let [canvas (-> event .getSurface .getCanvas)
        {:keys [x y]} (:mouse state)
        objects (collage/sprites state x y)]
    (.clear canvas (unchecked-int 0xFFFFFFFF))
    (doseq [object objects]
      (draw-object canvas object))
    (comment (doseq [[id item] (:items state)]
               (.drawImageRect canvas sprite (.offset sprite-rect (:x item) (:y item))))
             (draw-toolbar canvas false))))

(defn on-move
  [event]
  (def state (state/move-mouse state (.getX event) (.getY event)))
  (request-frame))

(defn on-press
  [event]
  (let [x (.getX event)
        y (.getY event)
        rect (-> sprite-rect (.withLeft 0) (.withTop 0))]
    (println "Ho" (collage/object-under-pos state x y) state x y)
    (println)
    (when-let [[type object] (and (-> state :dragging not) (collage/object-under-pos state x y))]
      (print "Hi")
      (case type
        :tool (def state (state/create-and-drag-item state x y))
        :item (def state (state/grab state object))
        (println type)))
    (comment (when (and (-> state :dragging not) (.contains rect x y))
               (def state (state/create-and-drag-item state x y))))))

(defn on-release
  [event]
  (def state (state/release state))
  (request-frame))

(defn on-button
  [event]
  (if (.isPressed event)
    (on-press event)
    (on-release event)))

(defn on-event
  [event]
  (condp instance? event
    EventFrameSkija (on-paint event)
    EventMouseMove (on-move event)
    EventMouseButton (on-button event)
    nil))

(defn listener
  [w]
  (consumer on-event))

(defn init []
  (doui (.setLayer w (LayerGLSkija.))
        (.setEventListener w (listener w))))

(defn show []
  (doui (.setVisible w true)))

(defn hide []
  (doui (.setVisible w false)))
