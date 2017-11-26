(ns util.math
  )

#?(:clj
   (do
     (defn sqrt [^double a] (Math/sqrt a) )
     (defn cos [^double a](Math/cos a))
     (defn sin [^double a](Math/sin a)))
   
   :cljs
   (do
     (defn sqrt [^double a] (.sqrt js/Math a))
     (defn cos [^double a] (.cos js/Math a))
     (defn sin [^double a] (.sun js/Math a))))

(defn align [alignment n]
  (let [r (rem n 16)]
    (if (= r 0)
      n
      (+ n (- 16 r) ))))
