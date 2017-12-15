(ns eggs.fastlines
  (:require-macros

    [cljs.core.async.macros :refer [go go-loop ]] )

  (:require

   [thi.ng.math.core :as m]
    [util.vec4 :refer [vec4]]
    [eggs.glvertbuffer :refer [mk-vert-buffer!]]
    [cljs.pprint :refer [pprint]]
    [util.misc :refer [map-keys]]
    [eggs.fontdata :refer [test-vec-font-line-strips]]
    [eggs.shaders :refer [async-load-shader]]
    [cljs.core.async :as async :refer [chan] ]
    [thi.ng.geom.matrix :as mat]
    [thi.ng.dstruct.streams :as streams]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]
    [thi.ng.geom.gl.webgl.constants :as glc]))

(defprotocol ITexture 
  (make-active! [_])
  (upload! [_ view] )
  (get-dims [_]))

(defrecord Texture [gl w h tex-id int-format src-type src-format ]
  ITexture
  (make-active! [this]
    (.bindTexture gl (.-TEXTURE_2D gl) tex-id)
    this)
  (upload! [this array-view] 
    (let [this (make-active! this)]
      (do
        ; (.bindBuffer gl (.-PIXEL_UNPACK_BUFFER gl) 0)
        (.bindTexture gl (.-TEXTURE_2D gl) tex-id)
        (.texImage2D gl 
                     (.-TEXTURE_2D gl)  ;; target
                     0                  ;; mip level
                     int-format         ;; internal format
                     w                  ;; width
                     h                  ;; height
                     0                  ;; boder
                     src-format         ;; src forna
                     src-type           ;; type
                     array-view         ;; array view
                     )
        this )))
  (get-dims [this] [w h]))

(defn as-2-v [v] 
  (when v (if (sequential? v ) v  [v v ]) ))

(defn set-tex-flags! [gl {:keys [wrap  filter ]} ]
  (when-let [[ w-s w-t ] (as-2-v filter)]
    ; (.texParameteri gl glc/texture-2d glc/texture-wrap-s w-s) 
    ; (.texParameteri gl glc/texture-2d glc/texture-wrap-t w-t)
    )

  (when-let [[min mag] (as-2-v filter)]
    (.texParameteri gl glc/texture-2d glc/texture-min-filter min)
    (.texParameteri gl glc/texture-2d glc/texture-mag-filter mag))
  )

(defn mk-texture [gl w h buffer-view flags]
  (let [tex-id (.createTexture gl) 
        def-flags {:filter glc/nearest
                   :wrap glc/clamp-to-edge 
                   :gl gl 
                   :w w :h h 
                   :tex-id tex-id } ]
    (doto gl 
      (.bindTexture (.-TEXTURE_2D gl) tex-id)
      (set-tex-flags!  flags))
    (->
      (map->Texture (merge def-flags flags) )
      (upload! buffer-view)))) 

(defn mk-font-texture [gl]
  (let [w 512 h 1024
        ft (mk-texture gl 256 1024 (js/Float32Array. (* w h 4)) 
                       {:int-format (.-RGBA32F gl)
                        :src-type   (.-FLOAT gl)
                        :src-format (.-RGBA gl)
                        :filter     glc/nearest
                        :wrap       glc/clamp-to-edge })

        st (mk-texture gl 28 40 (js/ByteArray. (* w h 4)) 
                       {:int-format (.-RGBA8UI gl)
                        :src-type   (.-BYTE gl)
                        :src-format (.-RGBA gl)
                        :filter     glc/nearest
                        :wrap       glc/clamp-to-edge
                        :array      (js/ByteArray. (* w h 4)) }) ]))

(defn strips->lines 
  "take collection of points that make a line strip 
   and turn it into a list of lines"
  [line-str]
  (loop [lines []
         line-str line-str]
    (if (> (count line-str) 1)
      (recur
        (into lines (take 2 line-str))
        (rest line-str))
      lines)))

(defn letter->lines 
  "take a collection of line strips and turn 
   them into a flat vector of lines"
  [line-strips] 
  (->
    strips->lines
    (mapcat line-strips) 
    vec))

(defn largest-col-in-map 
  "find the biggest collection in a map of collections
   return the size of that collection"
  [mp]
  (->> mp vals (apply max-key count) count ) )

(defn write-lines! [buffer offset verts]
  (doseq [i (range (count verts ))]
    (let [dest-idx (+ offset (* 2 i) )
          src-vert (nth verts i) ]
      (do 
        (aset buffer dest-idx (first src-vert) )
        (aset buffer (+ dest-idx 1) (second src-vert) )))))

(defn mk-line-bitmap [letters]
  (let [letters-v (into [] letters)
        elems-per-pix 4
        bytes-per-elem 4
        w (* (largest-col-in-map letters) elems-per-pix) 
        h (count letters)
        array-buffer (js/ArrayBuffer. (* h w bytes-per-elem) )
        float-buffer (js/Float32Array. array-buffer) 
        lines-info (loop [y 0 lines-info {} ]
                     (if (= y h)
                       lines-info
                       (let [[letter-key letter] (nth letters-v y)

                             let-rec   {:ypos y :num-of-lines (count letter) } ]
                         (write-lines! float-buffer (* y w) letter)
                         (recur (inc y)
                                (assoc lines-info letter-key let-rec))))) ]
    {:w w 
     :h h
     :buffer       array-buffer 
     :float-buffer float-buffer
     :lines-info   lines-info}))

(defn mk-lines-as-texture [gl]
  (let [letters (map-keys letter->lines test-vec-font-line-strips)
        {:keys [w h float-buffer] :as bit-map} (mk-line-bitmap letters )
        texture (mk-texture gl (/ w 4) h float-buffer
                {:int-format (.-RGBA32F gl)
                 :src-type   (.-FLOAT gl)
                 :src-format (.-RGBA gl)
                 :filter     glc/nearest
                 :wrap       glc/clamp-to-edge })]
  {:texture texture 
   :lines-info (:lines-info bit-map) }))

(def attribs {:a_pos       :vec2 
              :a_spr       :int
              :a_pal_flags :int })

(def fast-line-shader-spec
  {:vs-file "shaders/fastlines.vs"
   :fs-file "shaders/line.fs"
   :common "shaders/common.glsl"

   :version 300

   :varying {:v_uv       :vec2
             :v_color    :vec4
             :v_radius   :float 
             :v_hardness :float}

   :uniforms {:u_proj         [:mat4 mat/M44]
              :u_view         [:mat4 mat/M44]
              :u_model        [:mat4 mat/M44]
              :u_hardness     [:vec2 [1.0 1.0]]
              :u_radii        [:vec2 [1.0 1.0]]
              :u_inner_color  :vec4
              :u_outer_color  :vec4 
              :u_tex          :sampler2D }

   :attribs  attribs })

(defprotocol ILineSprs
  (make-line-sprs-active! [this unis])
  (render! [this unis txt]))

(def verts (repeat (* 6 40) {:a_pos (vec2 0 0)
                             :a_spr 0
                             :a_pal_flags 0} ))


(def default-unis {:u_inner_color (vec4 1 1 1 1)
                      :u_outer_color (vec4 1 1 1 1) })

(defrecord LineSprs [gl shader vb]
    ILineSprs
    (make-line-sprs-active! [this unis]
      )
    (render! [this unis txt]) 
  )

(defn mk-line-spr-ch [gl]
  (let [{:keys [texture lines-info]} (mk-lines-as-texture gl)
        tex-id (:tex-id texture ) 
        vb (mk-vert-buffer! gl attribs verts)  ]
    (go
      (let [shader (<! (async-load-shader gl fast-line-shader-spec))]
        (do
          (->LineSprs gl shader vb))
        ))))





