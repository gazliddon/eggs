(ns eggs.glvertbuffer
  (:require 
    [cljs.pprint :refer [pprint]]
    [util.misc :refer [js-log]]
    [eggs.protocols :as p]
    [eggs.vdef :as vdef]
    [thi.ng.geom.gl.webgl.constants :as glc]
    [util.misc :refer [map-kv map-keys]]))

;; TODO all glinfo in this file please

; (def gl-type-info 
;   {:int      {:gl-type glc/int :gl-vert-attr-ptr ivert-attrib-ptr  } 
;    :uint      {:gl-type glc/unsigned-int :gl-vert-attr-ptr ivert-attrib-ptr  }
;    :float    {}  
;    :vec2     {}
;    :vec3     {}
;    :vec4     {} })


;; TODO move gl-vert-attr-ptr and all GL bullshit into here from vdef
(defn mk-gl-attr [attr-spec]
  (let [{:keys [normalized? size stride offset gl-vert-attr-ptr gl-type]} attr-spec ]
    (reify p/IGLAttribute 
      (enable-attribute! [_ gl loc]
        (doto gl
            (.enableVertexAttribArray loc)
            (gl-vert-attr-ptr loc size gl-type normalized? stride offset))))))

(defn mk-gl-vert-buffer [{:keys [attr-specs array-buffer] :as vert-buffer } gl]
  (-> vert-buffer
      (assoc :gl-buffer (.createBuffer gl)
             :gl-attrs (map-keys mk-gl-attr attr-specs)
             :gl-buffer-view (js/Uint8Array. array-buffer))))

(extend-type vdef/VertBuffer 
  p/IGLVertBuffer

  (buffer-data! [{:keys [gl-buffer gl-buffer-view]} gl ]
    (.bindBuffer gl glc/array-buffer gl-buffer)
    (.bufferData gl glc/array-buffer gl-buffer-view glc/static-draw 0 0))

  (make-active! [{:keys [gl-attrs gl-buffer] :as this} gl {:keys [attribs] :as shader}]
    (do
      (.bindBuffer gl glc/array-buffer gl-buffer)
      (doseq [[id gl-attr] gl-attrs]
        (when-let [loc (get attribs id)]
          (p/enable-attribute! gl-attr gl loc))))))

(defn mk-vert-buffer [gl attribs n]
  (let [ret (-> (vdef/mk-vert-buffer attribs n) 
                (mk-gl-vert-buffer gl)) ]
    ret))


