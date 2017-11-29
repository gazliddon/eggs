(ns eggs.glwindow
  (:require 
    [thi.ng.geom.gl.camera :as cam]
    [eggs.printables :as pt]

    [goog.dom :as gdom] 


    [thi.ng.geom.core :as geom]
    [thi.ng.geom.triangle :as tri] 
    [thi.ng.geom.quad :as quad] 
    [thi.ng.geom.gl.glmesh :as glmesh]
    [thi.ng.geom.gl.shaders :as shaders] 
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]
    ))

(defn get-win-wh []
  (let [win js/window
        w (.-innerWidth win)
        h (.-innerHeight win)
        ar (/ w h) ]
    {:dims (vec2 w h)
     :aspect ar }))

(defn gl-context
  ([canvas attribs contexts]
   (let [canvas  (if (string? canvas) (.getElementById js/document canvas) canvas)
         attribs (clj->js attribs)
         ctx     (loop [ids contexts]
                   (when ids
                     (try
                       (let [ctx (.getContext canvas (first ids) attribs)]
                         (set! (.-onselectstart canvas) (constantly false))
                         (if ctx ctx (recur (next ids))))
                       (catch js/Error e (recur (next ids))))))]
     (or ctx (println "WebGL not available")))))

(defrecord GLWindow [ctx cam ])

(def contexts ["webgl2" ])

(def context-default-attribs
     {:alpha                                true
      :antialias                            true
      :depth                                true
      :fail-if-major-performance-caveat     false
      :prefer-low-power-to-high-performance false
      :premultiplied-alpha                  true
      :preserve-drawing-buffer              false
      :stencil                              false})

(defonce shader-spec
  {:vs "void main() { gl_Position = proj * view * model * vec4(position, 1.0); }"

   :fs "void main() { gl_FragColor = vec4(0.5, 0.5, 1.0, 1.0); }" 

   :uniforms {:view  :mat4
              :proj  :mat4
              :model :mat4 
              :col :vec4 }

   :attribs  {:position   :vec3 }})

(def triangle (geom/as-mesh (tri/triangle3 [[0.1 0 0] [-0.1 0 0] [0 0.1 0]])
                            {:mesh (glmesh/gl-mesh 3)}))

(def quad (geom/as-mesh (quad/quad3 [0.1 -0.1 ] [-0.1 -0.1 ] [-0.1 0.1 ] [0.1 0.1 ])
                            {:mesh (glmesh/gl-mesh 3)}))


(defn mk-gl-window [canvas-name]
  (let [ctx (gl-context canvas-name context-default-attribs contexts) ]
    (->GLWindow ctx nil)))
