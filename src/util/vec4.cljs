(ns util.vec4
  (:require)
  )

(defn vec4 [x y z w]
  (js/Float32Array. [x y z w]))
