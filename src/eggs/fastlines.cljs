(ns eggs.fastlines
  (:require
    [util.misc :refer [map-keys]]
    [eggs.fontdata :refer [test-vec-font-line-strips]]
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
  (doseq [i (range (count verts))]
    (let [dest-idx (+ offset (* 4 i) )]
      (comment .set buffer (nth verts i)))))

(defn mk-line-bitmap [line-strips]
  (let [elems-per-pix 4
        bytes-per-elem 4
        letters  (into [](map-keys letter->lines line-strips))
        w (* (largest-col-in-map letters) elems-per-pix) 
        h (count letters)
        array-buffer (js/ArrayBuffer. (* h w bytes-per-elem) )
        float-buffer (js/Float32Array. array-buffer) 

        lines-info (loop [y 0 lines-info {} ]
                   (if (= y h)
                     lines-info
                     (let [[letter-key letter] (nth letters y)
                           let-rec   {:ypos y :num-of-lines (count letter) } ]

                       (write-lines! float-buffer (* y w) letter)

                       (recur (inc y)
                              (assoc lines-info letter-key let-rec))))) ]
    {:w w :h h
     :buffer     array-buffer 
     :float-buffer float-buffer
     :lines-info  lines-info}))

(defn mk-lines-as-texture [gl]
  (let [letters (map-keys letter->lines test-vec-font-line-strips)
        {:keys [w h float-buffer] :as bit-map} (mk-line-bitmap letters )
        _ (println (.-length float-buffer))
        texture (mk-texture gl (/ w 4) h float-buffer
                {:int-format (.-RGBA32F gl)
                 :src-type   (.-FLOAT gl)
                 :src-format (.-RGBA gl)
                 :filter     glc/nearest
                 :wrap       glc/clamp-to-edge })]
  {:texture texture 
   :lines-info (:lines-info bit-map) }))



