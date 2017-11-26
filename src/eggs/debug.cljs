(ns eggs.debug

  (:require
            ))

(defonce debug-e (.getElementById js/document "debug"))

(defn set-debug[txt]
  (aset debug-e "innerHTML" txt))
