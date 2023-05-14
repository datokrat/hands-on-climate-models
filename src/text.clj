(ns text
  (:import
   [io.github.humbleui.skija FontMgr Font FontStyle Paint]))

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
