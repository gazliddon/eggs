(ns util.macros
  (:require [clojure.pprint :refer [pprint]]))

(def buff-body 
  '(cljs.core/IIndexed

     (-nth 
       ([o n nf] (js-nth o n nf))
       ([o n] (nth o n nil )))

     cljs.core/IEquiv
     (^boolean -equiv [o other]
               (zero? (compare o other)))

     cljs.core/ICounted 
     (-count [o]
             (js-count o))

     cljs.core/IComparable
     (-compare [o other] 
               (compare-indexed o other))))

(defmacro buff-js-array  [ty count-fn nth-fn]
  `(extend-type ~ty ~@buff-body ))





