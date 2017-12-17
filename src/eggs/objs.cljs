(ns eggs.objs
  (:require 
    [cljs.pprint :refer [pprint]]
    )
  )

(defn mk-objs []
  {})

(defprotocol IObj 
  (draw-obj [this r])
  (update-obj [this dt input ])
  (get-id [this]))

(defn add-obj [objs typ obj] 
  (let [id (get-id obj)
        new-objs (assoc-in objs [typ id] obj) ]
    (pprint new-objs)
    new-objs))

(defn map-objs [objs typ func]
  (assoc objs
         typ
         (map func (get typ objs))))
