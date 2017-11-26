(ns eggs.vertbuilder
  (:require)
  
  )


; (def gl-f32-array js/Float32Array)
; (def gl-u16-array js/Uint16Array)
; (def gl-u32-array js/Uint32Array)

; (defprotocol IDraw 
;   (attribs! [_ attribs])
;   (move! [_ pos])
;   (draw! [_ pos]))


; (defprotocol IVerts
;   (add-vert! [_ v])
;   (get-num-of-verts [_])
;   (get-vert [_ n]))


; (defrecord Drawer [a-attribs a-verts default]
;   IVerts
;   (add-vert! [_ v]
;     (swap! a-verts conj v))

;   (get-num-of-verts [_]
;     (count @a-verts))

;   (get-vert [_ n]
;     "WHOOPS")

;   IDraw
;   (attribs! [this attribs]
;     (swap! a-attribs merge attribs)
;     this)

;   (move! [this pos]
;     this)

;   (draw! [this pos]
;     this) )

; (defn mk-drawer []
;   (map->Drawer {:attribs {:color [1 1 1 0]
;                           :radii 1 }
;                 :default {:a_index      0
;                           :a_position0  [0 0]
;                           :a_position1  [0 0]
;                           :a_radii      [0 0]}
;                 :pos [0 0] })
;   )



; (def default {:col [1 1 1 0]
;               :pos [0 0]
;               :verts []})


(defn vert [{:keys [col pos verts] :as this}]
  (update this verts (conj {:col col :pos pos })))

(defn col [this c]
  (assoc this :col c))

(defn pos [this p]
  (assoc this :pos p))

(defn line-to [this p0]
  (->
    this
    (vert)
    (pos p0)
    (vert)))

(defn line [this p0 p1]
  (->
    this
    (pos p0)
    (line-to p1)))

(defn lines [this & ps]
  (let [this (-> this (pos (first ps)) (vert))]
    (->
      (reduce 
        (fn [{:keys [pos] :as res} p]
          (line res pos p)) 
        this (rest ps)))))

(defn line-loop [this ps]
  (->
    this
    (lines this ps)
    (line-to (first ps))))
