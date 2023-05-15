(ns util)

(defmacro defmethods [multifn dispatch-vals & fn-tail]
  `(doseq [dispatch-val# ~dispatch-vals]
     (defmethod ~multifn dispatch-val# ~@fn-tail)))

(defmacro doseqi
  [i seq-exprs & body]
  `(with-local-vars [i# 0]
    (doseq ~seq-exprs
      (let [~i @i#
            ret# (do ~@body)]
        (var-set i# (inc @i#))
        ret#))))

(defmacro defmulti [name & args]
  `(do
     (def ~name nil)
     (clojure.core/defmulti ~name ~@args)))
