(ns eggs.flatpoly
  )

(def flat-poly-shader-spec
  {:vs-file "shaders/flat.vs"
   :fs-file "shaders/flat.fs"

   :common "shaders/common.glsl"

   :version 300

   :varying {:v_color  :vec4
             :v_normal :vec3 
             :v_pos    :vec3 }

   :uniforms {:u_proj         :mat4
              :u_view         :mat4
              :u_model        :mat4 
              :u_light        :vec3
              :u_light_dir    :vec3 }

   :attribs  {:a_pos        :vec3
              :a_normal     :vec3
              :a_color      :vec4
              :a_splode_dir :vec3 }})



