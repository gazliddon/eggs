(ns util.stats
  (:require 
    [goog.dom :as gdom] 
    [cljsjs.stats]))

(defprotocol IStats
  (show [_])
  (hide [_])
  (begin [_])
  (end [_]))

(defrecord Stats [js-stats]
  IStats
  (begin [_]
    (.begin js-stats))
  (end [_]
    (.end js-stats))
  (show [_]
    (.showPanel js-stats 2))
  (hide [_]))

(defn mk-stats []
  (let [stats (js/Stats.)]
    (do 
      (.showPanel stats 0)
      (gdom/appendChild (.-body js/document) (.-dom stats))
      (->Stats stats))))
