(ns eggs.fastlines
  (:require
    [util.misc :refer [map-keys]]
    [eggs.fontdata :refer [test-vec-font-line-strips]]
    [thi.ng.geom.gl.webgl.constants :as glc]))

(defprotocol ITexture 
  (make-active! [_])
  (refresh! [_ ] )
  (replace! [_ array] )
  (get-array [_])
  (get-dims [_]))

(defrecord Texture [gl w h array tex-id int-format src-type src-format]
  ITexture

  (make-active! [this]
    (.bindTexture gl 0 tex-id)
    this)

  (replace! [this array]
    (-> (assoc this :array array) 
        (refresh!)))

  (refresh! [this] 
    (let [this (make-active! this)]
      (do
        (.texImage2D gl (.-TEXTURE_2D gl) 0 int-format w h 0 src-format src-type array)   
        this )))
     
  (get-array [this] array)

  (get-dims [this] [w h]))

(defn as-2-v [v] 
  (when v (if (sequential? v ) v  [v v ]) ))

(defn set-tex-flags! [gl {:keys [wrap  filter ]} ]
  (when-let [[ w-s w-t ] (as-2-v filter)]
    (.texParameteri gl glc/texture-2d glc/texture-wrap-s w-s) 
    (.texParameteri gl glc/texture-2d glc/texture-wrap-t w-t))

  (when-let [[min mag] (as-2-v filter)]
    (.texParameteri gl glc/texture-2d glc/texture-min-filter min)
    (.texParameteri gl glc/texture-2d glc/texture-min-filter mag)  ))

(defn mk-texture [gl w h flags]
  (let [tex-id (.createTexture gl) 
        def-flags {:filter glc/nearest
                   :wrap glc/clamp-to-edge 
                   :gl gl 
                   :w w :h h } ]
    (doto gl 
      (.bindTexture 0 tex-id)
      (set-tex-flags!  flags))
    (->
      (map->Texture (merge def-flags flags))
      (refresh!)))) 

(defn mk-font-texture [gl]
  (let [w 512 h 1024
        ft (mk-texture gl 256 1024 {:int-format (.-RGBA32F gl)
                                    :src-type   (.-FLOAT gl)
                                    :src-format (.-RGBA gl)
                                    :filter     glc/nearest
                                    :wrap       glc/clamp-to-edge 
                                    :array      (js/Float32Array. (* w h 4)) })

        st (mk-texture gl 28 40 {:int-format (.-RGBA8UI gl)
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
      (comment .set buffer (nth verts i)
            
            ))))

(defn mk-line-bitmap [line-strips]
  (let [letters  (into [](map-keys letter->lines line-strips))
        w (* (largest-col-in-map letters) 4) 
        h (count letters)
        array-buffer (js/ArrayBuffer. (* h w)) 
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
     :lines-info  lines-info}))

(defn mk-lines-as-texture [gl]
  (let [letters (map-keys letter->lines test-vec-font-line-strips)
        {:keys [w h buffer] :as bit-map} (mk-line-bitmap letters )
        texture (mk-texture gl (/ w 2) h
                {:int-format (.-RGBA32F gl)
                 :src-type   (.-FLOAT gl)
                 :src-format (.-RGBA gl)
                 :filter     glc/nearest
                 :wrap       glc/clamp-to-edge 
                 :array      buffer }) ]
  {:texture texture 
   :lines-info (:lines-info bit-map) }))



