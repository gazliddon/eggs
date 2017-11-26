(ns eggs.glwindow
  (:require 
    [thi.ng.geom.gl.camera :as cam]
    [eggs.printables :as pt]

    [thi.ng.geom.core :as geom]
    [thi.ng.geom.triangle :as tri] 
    [thi.ng.geom.quad :as quad] 
    [thi.ng.geom.gl.glmesh :as glmesh]
    [thi.ng.geom.gl.shaders :as shaders] 
    ))


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

(defonce line-shader-spec
  {:vs "void main() { gl_Position = proj * view * model * vec4(position, 1.0); }"

   :fs "void main() { gl_FragColor = vec4(0.5, 0.5, 1.0, 1.0); }" 

   :uniforms {:u_vp       :mat4
              :u_model    :mat4
              :u_hardness :vec2
              :u_radii    :vec2}

   :attribs  {:a_index      :int
              :a_position0  :vec2
              :a_position1  :vec2
              :a_radii      :vec2
              :a_color0     :vec4
              :a_color1     :vec4 }})

(defn mk-gl-window [canvas-name]
  (let [ctx (gl-context canvas-name context-default-attribs contexts)
        cam (cam/perspective-camera {}) ]
    (do
      (pt/register-printable! ctx :tri triangle shader-spec)
      (pt/register-printable! ctx :quad quad shader-spec))
    (->GLWindow ctx cam)))
