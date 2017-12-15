(ns eggs.fontvb
  (:require 
    [cljs.pprint :refer [pprint]]
    [thi.ng.geom.core :as g]

    [util.vec4 :refer [vec4]]

    [eggs.glvertbuffer :as glvb :refer [mk-vert-buffer!]]
    [eggs.glhelpers :refer [gl-clear! set-uni! set-unis! use-program!] ]
    [eggs.fontdata :as fdata]
    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf ]]
    [eggs.protocols :as p]
    [eggs.lines :refer [add-lines mk-line-verts]]

    [thi.ng.geom.gl.webgl.constants :as glc]
    [thi.ng.geom.matrix :as mat]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

(enable-console-print!)

(defn to-verts [l-chunk] (map #(apply vec3 %) l-chunk))

(defn add-font-character [verts nm lines] 
  (let [start (count (:verts verts))
        new-verts (->> (map to-verts lines)
                       (reduce add-lines verts))
        num-of-verts (- (count (:verts new-verts) ) start)  ]

    (update-in new-verts [:frames nm] assoc 
               :start start 
               :num-of-verts num-of-verts)))

(defn mk-font-verts [font-data base-vert]
  (let [verts (mk-line-verts base-vert)]
    (reduce-kv add-font-character verts font-data)))

(defprotocol IText
  (start-text [this shader unis])
  (print-it-mat [this mat col text]) 
  (print-it [this pos col text]))

(def *gl-active* (atom {}))

(defprotocol IDrawable 
  (start! [_ gl shader unis] )
  (draw-tris [_ gl start num-of-verts])
  (stop! [_ gl]))


(defrecord Font [gl attribs shader-atom base-unis vb frames]
      IText
      (start-text [_ shader uniforms]
        (do 
          (reset! shader-atom shader)
          (use-program! gl shader)
          (p/make-active! vb gl shader)
          (set-unis! gl @shader-atom (merge base-unis uniforms))))

      
      (print-it-mat [this model col text]
        (let [frm  (get frames text)
              start (:start frm)
              num-of-verts (:num-of-verts frm) ]
          (do 
            (set-uni! gl @shader-atom :u_model model)  
            (set-uni! gl @shader-atom :u_inner_color col)  
            (set-uni! gl @shader-atom :u_outer_color col)  
            (.drawArrays gl glc/triangles start num-of-verts)   
            )
          ) 
        ) 

      (print-it [_ pos col text]
        (let [frm  (get frames text)
              start (:start frm)
              num-of-verts (:num-of-verts frm) 
              model (-> mat/M44 (g/translate pos)) ]
          (do 
            (set-uni! gl @shader-atom :u_model model)  
            (set-uni! gl @shader-atom :u_inner_color col)  
            (set-uni! gl @shader-atom :u_outer_color col)  
            (.drawArrays gl glc/triangles start num-of-verts)   
            )
          )) 
  )

(defn mk-font [gl attribs]
  (let [base-vert {:a_radii  (vec2 0.2 0.2)
                   :a_color0 (vec4 1 1 1 0.8)
                   :a_color1 (vec4 1 1 1 0.8)}
        {:keys [frames verts]} (mk-font-verts fdata/vec-font-line-strips base-vert) ]

    (map->Font {:gl gl

                :attribs attribs

                :shader-atom (atom nil)

                :base-unis {:u_model mat/M44
                            :u_hardness (vec2 1 1)   
                            :u_outer_color (vec4 1 1 1 1)
                            :u_inner_color (vec4 1 1 1 1)
                            :u_radii (vec2 1 1)}
                :vb (mk-vert-buffer! gl attribs verts)
                :frames frames})))

