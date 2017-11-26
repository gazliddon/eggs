(ns util.protocols
  (:refer-clojure :exclude [+ - * ])
  )

(defprotocol IMathOps
  (+ [a b])
  (- [a b])
  (* [a b])
  (div [a b]) )


