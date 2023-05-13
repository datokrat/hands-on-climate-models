(ns user
  (:require
   state collage scene item
   [nrepl.cmdline :as nrepl])
  (:use util)
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

(def sun-sprite
  (str->sprite "./img/sun.png"))

(def earth-sprite
  (str->sprite "./img/earth.png"))

(def state
  (state/initial))

(defn request-frame []
  (doui (.requestFrame w)))

(defn type->sprite [type]
  (case type
    :sun sun-sprite
    :earth earth-sprite))

(defn draw-toolbar
  [canvas tool hovered?]
  (.drawImageRect canvas (type->sprite tool) (collage/tool-rect-scaled
                                              tool (if hovered? 1.2 1))))

(defmulti draw-item (fn [canvas item hovered? frame-data] (item/type item)))

(defmethod draw-item :sun
  [canvas item hovered? frame-data]
  (.drawImageRect canvas sun-sprite (item/bounding-box item frame-data)))

(defmethod draw-item :earth
  [canvas item hovered? frame-data]
  (.drawImageRect canvas earth-sprite (item/bounding-box item frame-data)))

(def font-mgr (FontMgr/getDefault))
(def typeface (.matchFamilyStyle font-mgr "Liberation Sans" FontStyle/NORMAL))
(def font (Font. typeface (float 20)))

(defn draw-text
  [canvas object]
  (with-open [fill (Paint.)]
    (.drawString canvas
                 (-> object :text)
                 (-> object :pos :x)
                 (-> object :pos :y)
                 font
                 fill)))

(defn draw-object
  [canvas object frame]
  (case (:type object)
    :tool (draw-toolbar canvas (:tool object) (:hovered? object))
    :item (draw-item canvas (:item object) (:hovered? object) (get frame (:id object)))
    :text (draw-text canvas object)))

(defn draw-time-slider
  [canvas]
  (let [t (-> state state/get-scene scene/get-time)
        x (+ 100 (* t 50))
        knob-rect (rect/offset (rect/centered-square 30) x 655)]
    (with-open [fill (Paint.)]
      (.setColor fill (unchecked-int 0xFFCCCCCC))
      (.drawRect canvas (rect/ltrb 100 650 600 660) fill)
      (.setColor fill (unchecked-int 0xFFFF0000))
      (.drawOval canvas knob-rect fill))))

(defn on-paint
  [event]
  (let [canvas (-> event .getSurface .getCanvas)
        {:keys [x y]} (:mouse state)
        objects (collage/sprites state x y)
        frame (-> state state/get-scene scene/get-frame)]
    (.clear canvas (unchecked-int 0xFFEEEEEE))
    (with-open [fill (Paint.)]
      (.setColor fill (unchecked-int 0xFFFFFFFF))
      (.drawRect canvas state/clip-rect2 fill))
    (doseq [object objects]
      (draw-object canvas object frame))
    (draw-time-slider canvas)))

(defn on-move
  [event]
  (def state (state/move-mouse state (.getX event) (.getY event)))
  (request-frame))

(defn on-press-time-slider [event]
  (when (.contains (rect/ltrb 100 650 600 660) (.getX event) (.getY event))
    (state/update-scene state #(scene/set-time % (/ (- (.getX event) 100) 50)))))

(defn on-press
  [event]
  (let [x (.getX event)
        y (.getY event)]
       (if-let [[type object] (collage/object-under-pos state x y)]
         (case type
           :tool (def state (state/grab-tool state object x y))
           :item (def state (state/grab state object))
           :property-editor nil)
         (def state (or (on-press-time-slider event)
                        (state/deselect state))))
       (request-frame)))

(defn on-release
  [event]
  (def state (state/mouse-released state))
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
