(ns eggs.lineshader
(:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] )
  (:require
    [eggs.fetch :refer [fetch-files-in-hash]]
    [cljs.core.async :as async ]
    [thi.ng.geom.gl.shaders :as shaders] ))

(defn async-load-shader [ gl shader ]
  (go
    (let [kz [:vs-file :fs-file :common]
          loaded (-> (select-keys shader kz)
                     (fetch-files-in-hash)
                     (async/<!)) 
          {:keys [common vs-file fs-file]} loaded 

          shader (assoc  shader
                        :vs (str common "\r\n" vs-file)
                        :fs (str common "\r\n" fs-file)
                        :vs-file nil
                        :fs-file nil
                        :common nil) ]
      (shaders/make-shader-from-spec gl shader ))))



(def line-shader-spec
  {:vs-file "shaders/line.vs"

   :fs-file "shaders/line.fs"

   :common "shaders/common.glsl"

   :version 300

   :varying {:v_uv :vec2
             :v_color :vec4
             :v_radius :float 
             :v_hardness :float}

   :uniforms {:u_vp           :mat4
              :u_model        :mat4
              :u_hardness     [:vec2 [1.0 1.0]]
              :u_radii        :vec2
              :u_inner_color  :vec4
              :u_outer_color  :vec4 }

   :attribs  {:a_index      :int
              :a_position0  :vec3
              :a_position1  :vec3
              :a_radii      :vec2
              :a_color0     :vec4 
              :a_color1     :vec4 }})
