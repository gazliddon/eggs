(ns eggs.fontvb
  (:require 
    [cljs.pprint :refer [pprint]]

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

;; 99 move to a new pos / 999 = end of data
(def i->token {99 :marker 999 :marker} )

(defn mk-letter [l-vec]
  (->>
    l-vec
    (map #(get i->token % %))
    (partition-by keyword?)
    (remove (fn [[k]] (keyword? k)) )
    ;; end up with a vector of vectors of line strips
    (map #(partition 2 %))))

(defn add-font-character [verts nm lines] 
  (let [start (count (:verts verts))
        new-verts (->> (mk-letter lines)
                       (map to-verts)
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
  (print-it [this pos col text]))

(def *gl-active* (atom {}))

(defprotocol IDrawable 
  (start! [_ gl shader unis] )
  (draw-tris [_ gl start num-of-verts])
  (stop! [_ gl]))

(defn mk-font [gl attribs]
  (let [base-vert {:a_radii  (vec2 0.2 0.2)
                   :a_color0 (vec4 1 1 1 0.8)
                   :a_color1 (vec4 1 1 1 0.8)}

        {:keys [frames verts]} (mk-font-verts fdata/test-font-data base-vert) 

        vb (mk-vert-buffer! gl attribs verts) 
        frm (:B frames) 
        base-unis {:u_model mat/M44
                   :u_hardness (vec2 1 1)   
                   :u_outer_color (vec4 1 1 1 1)
                   :u_inner_color (vec4 1 1 1 1)
                   :u_radii (vec2 1 1)}  ]
    (pprint verts)

    (t/info "creating font-data")
    (reify 
      IText
      (start-text [_ shader uniforms]
        (do 
          (use-program! gl shader)
          (p/make-active! vb gl shader)
          (set-unis! gl shader (merge base-unis uniforms))))

      (print-it [_ pos col text]
        (let [frm frm
              start (:start frm)
              num-of-verts (:num-of-verts frm) ]
          (.drawArrays gl glc/triangles start num-of-verts))))
    ))


