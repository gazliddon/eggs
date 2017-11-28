(ns eggs.protocols 
  (:require))

(defprotocol IKeys
  (flush-keys! [_])
  (get-key [_ k]))

(defprotocol IGL
  (clear [_ col])
  (draw [_ id uniforms]))

;{{{ Protocols

(defprotocol IBuffer 
  (read-buffer [_ n])
  (write-buffer! [_ n v]) )

(defprotocol IAttribute
  (mk-array [_ array-buffer n])
  (get-type-id [_])
  (get-offset [_])
  (get-element-size [_])
  (get-num-of-elements [_])
  (get-attr-size [_]))

(defprotocol IAttributeBuffer 
  (enable-array! [_])
  (disable-array! [_])
  (bind-array! [_]))

(defprotocol IVertDef 
  (get-attributes [_])
  (get-vert-size [_]))

(defprotocol IVertBuffer 
  (get-array-buffer [_])
  (get-num-of-verts [_] )
  (get-attr-buffers [_])
  (get-size-in-bytes [_]))

(defprotocol IGLVertBuffer 
  (make-active!  [this gl shader]) 
  (buffer-data!  [this gl ]))

(defprotocol IGLAttribute 
  (enable-attribute! [_ gl loc]))

;; }}}
