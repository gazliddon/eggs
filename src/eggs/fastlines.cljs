(ns eggs.fastlines
  (:require
    [thi.ng.geom.gl.webgl.constants :as glc]))

(defprotocol ITexture 
  (make-active! [_])
  (refresh! [_ ] )
  (replace! [_ array] )
  (get-array [_])
  (get-dims [_]))

(defrecord Texture [gl w h array tex-id int-format src-type src-format flags]
  ITexture

  (make-active! [this]
    (.bindTexture gl tex-id)
    this)

  (replace! [this array]
    (-> (assoc this :array array) 
        (refresh!)))

  (refresh! [this] 
    (let [this (make-active! this)]
      (do
        (.texImage2D gl 0 int-format w h 0 src-format src-type array)   
        this )))
     
  (get-array [this] array)

  (get-dims [this] [w h]))

(defn as-2-v [v] 
  (when v (if (sequential? v ) v  [v v ]) ))

(defn set-tex [gl {:keys [wrap  filter ]} ]
  (when-let [[ w-s w-t ] (as-2-v filter)]
    (.texParameteri gl glc/texture-2d glc/texture-wrap-s w-s) 
    (.texParameteri gl glc/texture-2d glc/texture-wrap-t w-t))

  (when-let [[min mag] (as-2-v filter)]
    (.texParameteri glc/texture-2d glc/texture-min-filter min)
    (.texParameteri glc/texture-2d glc/texture-min-filter mag)  ))

(defn mk-texture [gl w h]
  (let [tex-id (.createTexure gl) 
        flags {:filter glc/nearest
                 :wrap glc/clamp-to-edge } ]
    (doto gl 
      (.bindTexture tex-id)
      (set-tex  {:filter glc/nearest
                 :wrap glc/clamp-to-edge }))
    (->
      (map->Texture {:gl gl 
                     :w w :h h 
                     :array (js/Float32Array (* w h 4)) 
                     :tex-id tex-id 
                     :int-format (.-RGBA32F gl) 
                     :src-type glc/float
                     :src-format glc/float
                     :flags flags })
      (refresh!) ))) 

(defn mk-font-texture []
  )


