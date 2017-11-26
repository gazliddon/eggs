(ns eggs.pad
  (:require 
    [eggs.debug :refer [set-debug]]

    [thi.ng.geom.vector :as v :refer [vec2 vec3]]


    [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]

    [goog.dom :as gdom] 
    [goog.userAgent :as gagent] 
    [goog.events :as gevents]))

(enable-console-print!)

(defprotocol IPads
  (poll! [this])
  (amount? [this])
  (on-pad-added! [this e])
  (on-pad-removed! [this e]))

(defprotocol IPad
  (get-left-stick [_])
  (get-right-stick [_])
  (get-fire [_] )
  (get-fire-2 [_] )
  (update-pad [_ new-pad]))


(def joy-pad-cfg
  {:dead-zone-mag 0.3

   :pos-mul (vec2 1 -1)

   :left-stick-axis [0 1]
   :right-stick-axis [2 3]

   })

(defn- event->pad [event]
 (.-gamepad event))

(defn- get-axis [p n]
  (let [axes (.-axes p)]
    (aget axes n)))

(defn get-axis-pair [p a b]
  (m/* (:pos-mul joy-pad-cfg)
       (vec2 (get-axis p a)
             (get-axis p b))))

(defn- get-button [p n]
  (.-pressed (aget (.-buttons p) n)))


(defn abs [v]
  (if (< v 0 )
    (Math/abs v)
    v))

(defn sgn [v]
  (cond
    (< v 0) -1
    (> v 0) 1
    :else 0))

(defn dz-axis [dzm v ]
  (let [av (abs v) ]
    (if (>= av dzm)
      (* (sgn v) 
         (/ (- av dzm) (- 1.0 dzm)))
      0)))
 
(defn- 
  dead-zone 
  [xy ]
  (let [dzm (:dead-zone-mag joy-pad-cfg) ]
    (vec2 
      (dz-axis dzm (:x xy) )
      (dz-axis dzm (:y xy) ))))

(defrecord Pad [pad old-pad ]
  IPad
  (get-left-stick [_]
    (-> (:left-xy pad) (dead-zone)))

  (get-fire [_]
    (:fire pad))

  (get-fire-2 [_]
    (:fire-2 pad))

  (get-right-stick [_]
    (-> (:right-xy pad) (dead-zone)))

  (update-pad [_ new-p]
    (Pad. new-p pad)))

(defn-
  js->pad 
  "convert js pad record to something we like
  or just return a nuil pad if we haven't
  got a pad record"

  [p]
  (if p
    {:left-xy (get-axis-pair p 0 1)
     :right-xy (get-axis-pair p 2 3 )
     :fire (get-button p 0)
     :fire-2 (get-button p 1)
     :raw p }

    {:fire false
     :fire-2 false
     :left-xy (vec2 0 0)
     :right-xy (vec2 0 0)
     :raw nil
     }))

(defn- get-first-pad []
  (-> (gagent/getNavigator)
                (.getGamepads)
                (aget 0)
                (js->pad)))
(defn mk-pad [e]
  (let [js-p (js->pad e)]
    (->Pad js-p js-p)))

(defrecord Pads [all-pads]
  IPads

  (on-pad-added! [this e])

  (on-pad-removed! [this e])

  (poll! [this]
    (let [pad-rec (get-first-pad) ]
      (do
        (->Pad pad-rec pad-rec))))

  (amount? [this] 1))

(defn- attach! [obj event-name method]
  (do 
    (.addEventListener 
      (gdom/getWindow)
      event-name 
      (fn [event] (method obj event))))
  obj)

(defn mk-pads []
  (->
    (atom nil)
    (->Pads)))
