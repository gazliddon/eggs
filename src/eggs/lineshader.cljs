(ns eggs.lineshader
  (:require 
    ))

(def line-shader-spec
  {:vs-file "shaders/line.vs"
   :fs-file "shaders/line.fs"
   :common "shaders/common.glsl"

   :version 300

   :varying {:v_uv :vec2
             :v_color :vec4
             :v_radius :float 
             :v_hardness :float}

   :uniforms {:u_proj         :mat4
              :u_view         :mat4
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
