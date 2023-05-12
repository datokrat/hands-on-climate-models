(ns rect
  (:import
   [io.github.humbleui.types Rect]))

(defn xywh [x y w h]
  (Rect/makeXYWH x y w h))

(defn ltrb [l t r b]
  (Rect/makeLTRB l t r b))

(defn offset [rect x y]
  (.offset rect x y))

(defn scale [rect factor]
  (.scale rect factor))

(defn centered-square [length]
  (let [lower (- (/ length 2))]
    (rect/xywh lower lower length length)))
