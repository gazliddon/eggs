(ns eggs.glvertbuffer
  (:require 
    [eggs.protocols :as p]
    [eggs.vdef :as vdef]
    [thi.ng.geom.gl.webgl.constants :as glc]
    [util.misc :refer [map-kv map-keys]]))

(defn mk-gl-attr [attr-spec]
  (let [{:keys [normalized? size stride offset gl-vert-attr-ptr gl-type]} attr-spec ]
    (reify p/IGLAttribute 
      (enable-attribute! [_ gl loc]
        (doto gl
            (.enableVertexAttribArray loc)
            (gl-vert-attr-ptr loc size gl-type normalized? stride offset))))))

(defn mk-gl-vert-buffer [{:keys [attr-specs] :as vert-buffer } gl]
  (-> vert-buffer
      (assoc :gl-buffer (.createBuffer gl)
             :gl-attrs (map-keys mk-gl-attr attr-specs)) ))

(extend-type vdef/VertBuffer 

  p/IGLVertBuffer

  (make-active! [{:keys [gl-attrs gl-buffer] :as this} gl {:keys [attrs] :as shader}]
    (do
      (.bindBuffer gl glc/array-buffer gl-buffer)
      (doseq [[id gl-attr] gl-attrs]
        (when-let [loc (get attrs id)]
          (p/enable-attribute! gl-attr gl loc))))))

(defn mk-vert-buffer [gl attribs n]
  (-> (vdef/mk-vert-buffer attribs n) 
      (mk-gl-vert-buffer gl)))


