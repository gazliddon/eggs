(ns eggs.vdef
  (:require
    [eggs.protocols :as p]
      
    [thi.ng.geom.vector :as v :refer [vec2 vec3 ]]

    [util.vec4 :refer [vec4]]
    [util.misc :refer [map-keys ->pprint js-log equiv-indexed compare-indexed]]
    [util.math :refer [align]]

    [thi.ng.xerror.core :as err]  

    [cljs.pprint :refer [pprint]]
    [cljs.spec.alpha :as s]
    [thi.ng.geom.gl.webgl.constants :as glc]

    [taoensso.truss :as truss :refer (have have! have?)]
    [thi.ng.dstruct.streams :as streams]

    [taoensso.timbre :as t :refer-macros [info  ]])

  (:require-macros [util.macros :refer [buff-js-array]]))

(enable-console-print!)

;; {{{ TypeInfo

(defrecord TypeInfo [size array elements offset type writer reader ]
  p/IAttribute
  (mk-array [_ array-buffer n] 
    (array. array-buffer offset (* n elements) ))
  (get-type-id [_] type)
  (get-offset [_] offset)
  (get-element-size [_] size)
  (get-num-of-elements [_] elements)
  (get-attr-size [_] (* elements size)))

(defn ivert-attrib-ptr [gl loc size type _ stride offset]
  (.vertexAttribIPointer gl loc size type stride offset)  )

(def type-info-defaults
  {:elements 1 
   :array js/Float32Array
   :gl-type glc/float
   :gl-vert-attr-ptr :TBD
   :reader (fn [buff n ] (aget buff n))
   :writer (fn [buff n v] (aset buff n v)) })

(defn mk-type-info [type-id hsh]
  (let [hsh (merge type-info-defaults hsh)
        attr-spec {:type (:gl-type hsh)
                   :normalized? false
                   :size (:elements hsh) } ]


    (->
      (assoc hsh :type type-id :attr-spec attr-spec)
      (map->TypeInfo))))

;; }}}

;; {{{ Information about types

(declare type-info-records)

(defn get-type-info [id]
  (get type-info-records id))

(defn- id->size [id]
  (->
    (get-type-info id)
    (p/get-attr-size)))

(defn- vf-writer [buffer-view n src ] 
  (let [src (streams/get-float-buffer src) ]
    (.set buffer-view src n)))

(defn- arr-writer [buffer-view n src ] (.set buffer-view src n))

(def type-info 
  {:float    {:size 4 } 
   :int      {:size 4 :array js/Int32Array  :gl-type glc/int 
              :gl-vert-attr-ptr ivert-attrib-ptr  }

   :vec2     {:size 4 
              :elements 2 
              :writer vf-writer
              :reader (fn [buff n]
                      (vec2 
                        (aget buff n)
                        (aget buff (+ n 1)))) }

   :vec3     {:size 4 
              :elements 3 
              :writer vf-writer
              :reader (fn [buff n]
                      (vec3
                        (aget buff n)
                        (aget buff (+ n 1))
                        (aget buff (+ n 2))))}
   :vec4     {:size 4
              :elements 4 
              :writer arr-writer
              :reader (fn [buff n]
                      (vec4 
                        (aget buff (+ n 0))
                        (aget buff (+ n 1))
                        (aget buff (+ n 2))
                        (aget buff (+ n 3))))} })

(def type-info-records
  (reduce-kv
    (fn [o k v] (assoc o k (mk-type-info k v)))
    {} type-info))

;; }}}

;; {{{  Vert def making

(defn add-v-def [{:keys [size] :as vdef} id type-info]
  (->
    (assoc-in vdef [:vdefs id] (merge type-info {:offset size}))
    (assoc :size (+ size (p/get-attr-size type-info)) )))

(def ALIGNMENT 16)

(defrecord VertDef [attrs]
  cljs.core/ILookup
  (-lookup [o k not-found] (get attrs k not-found))
  (-lookup [o k] (get attrs k))

  p/IVertDef 
  (get-attributes [_] (:vdefs attrs ))
  (get-vert-size [_] 
    (align ALIGNMENT (:size attrs))))


(defn- sort-vert-map [v-def-map]
  (let [pred (fn [a b]
               (let [sz-a (id->size (get v-def-map a))
                     sz-b (id->size (get v-def-map b)) ]
                 (compare  [sz-b b] [sz-a a])))]
    (into (sorted-map-by pred) v-def-map)))

(defn mk-vdef 
  ([vdef info-unsorted]
   (t/info "making vertex definiton")
   (let [info (sort-vert-map info-unsorted)
         vd (-> (fn [vdef id type ]
                  (let [type-info (-> type-info-records type) ]
                    (add-v-def vdef id type-info) )) 
                (reduce-kv vdef info)
                (->VertDef)) ]
     (->
       (assoc vd :padded-size (p/get-vert-size vd)))))

  ([info]
   (mk-vdef {:size 0 :vdefs {} } info)))

;; }}}

;; {{{ Attribute Buffer Making

(defrecord AttributeBuffer[attr-def stride-elems buffer-view reader writer num-of-elems]

  cljs.core/IIndexed

  (-nth [this n not-found] 
    (if (or (>= n num-of-elems) (< n 0))
      not-found
      (p/read-buffer this n)))

  (-nth [this n] (-nth this n nil))

  p/IBuffer
  (read-buffer [this n]
    (reader buffer-view (* n stride-elems)))

  (write-buffer! [this n v]
    (writer buffer-view (* n stride-elems) v)
    this))

(defn mk-attr-spec [attr-def vert-def array-buffer]
  {:offset (p/get-offset attr-def) 
   :type (:gl-type attr-def)
   :buffer array-buffer
   :normalized? false
   :size (p/get-num-of-elements attr-def) 
   :stride (p/get-vert-size vert-def) })

(defn mk-attr-bufffer [{:keys [reader writer] :as attr-def} vert-def array-buffer n]
  (let [{:keys [stride offset]:as attr-spec} (mk-attr-spec attr-def vert-def array-buffer)
        element-size (p/get-element-size attr-def) ]

    (map->AttributeBuffer 
      {:attr-spec attr-spec
       :attr-def attr-def 
       :writer (:writer attr-def) 
       :reader (:reader attr-def)
       :stride-elems (/ stride element-size) 
       :num-of-elems n 
       :buffer-view (p/mk-array attr-def array-buffer n)})))

;; }}}

;; {{{  Vert Buffer Making

(defrecord VertBuffer [attr-buffers vert-def array-buffer num-of-verts]

  p/IVertBuffer

  (get-num-of-verts [_] num-of-verts)
  (get-size-in-bytes [this] (* (p/get-vert-size this) (p/get-num-of-verts this)))
  (get-attr-buffers [_ ] attr-buffers)
  (get-array-buffer [_] array-buffer)

  p/IVertDef 
  (get-attributes [_] (p/get-attributes vert-def))
  (get-vert-size [_] (p/get-vert-size vert-def)) 

  cljs.core/IIndexed
  (-nth [this n] 
    (nth this n nil))

  (-nth [this n not-found] 
    (p/read-buffer this n))

  p/IBuffer
  (read-buffer [_ n]
    (->
      (fn [acc attr-id attr-buffer]
        (assoc acc attr-id (p/read-buffer attr-buffer n )))
      (reduce-kv {} attr-buffers)))

  (write-buffer! [this n v]
    (doseq [[k v] v]
      (when-let [attr-buffer (get attr-buffers k)]
        (p/write-buffer! attr-buffer n v)))
        
    this))

(def memo-mk-vdef (memoize mk-vdef))

(defn mk-vert-buffer [vert-info n]
  (let [vert-def (memo-mk-vdef vert-info )
        stride (p/get-vert-size vert-def)
        array-buffer (js/ArrayBuffer. (* n stride))
        attr-defs (p/get-attributes vert-def) 
        attr-buffers (map-keys 
                       (fn [attr-def]
                         (mk-attr-bufffer attr-def vert-def array-buffer n))
                       attr-defs)]

    (t/info (str "creating buffer: " n " elements") )

    (map->VertBuffer {:attr-buffers attr-buffers
                      :vert-def vert-def 
                      :array-buffer array-buffer
                      :num-of-verts n})))
;; }}}

;; {{{ JS Utility Code

;; useful js extenders
;; TODO put in own file
;; write macro to gen code

(defn- js-count [o] (.-length o))

(defn- js-nth [o n not-found]
  (cond
    (< n 0) not-found
    (>= n (count o)) not-found
    :else (aget o n)))
;; }}}

;; {{{ JS Extend float array so it's comparable etc

(comment extend-type js/Float32Array
  cljs.core/IIndexed
  (-nth 
    ([o n nf] (js-nth o n nf))
    ([o n] (nth o n nil )))

  cljs.core/IEquiv
  (^boolean -equiv [o other] (zero? (compare o other)))

  cljs.core/ICounted (-count [o] (js-count o))

  cljs.core/IComparable
  (-compare [o other] (compare-indexed o other)))


(buff-js-array js/Float32Array js-count js-nth)
(buff-js-array js/Int32Array js-count js-nth)

;; }}}

;; {{{ Test code

(def line-vdef
  {:a_index      :int
   :a_position0  :vec2
   :a_position1  :vec2
   :a_radii      :vec2
   :a_color0     :vec4 
   :a_color1     :vec4 })

(comment
  (do 
    (def vb (mk-vert-buffer line-vdef 100))

    (defn log-buffer [id]
      (js-log (str "logging " (name id)))
      (-> vb p/get-attr-buffers (get id) pprint)
      (js-log "" )
      (js-log "" ))

    (def test-vert {:a_index      100
                    :a_position0  (vec2 0 1)
                    :a_position1  (vec2 2 3)
                    :a_radii      (vec2 4 5)
                    :a_color0     (vec4 6 7 8 9)
                    :a_color1     (vec4 10 11 12 13)})

    (pprint test-vert)

    (p/write-buffer! vb 0 test-vert)

    (pprint (p/read-buffer vb 0))

    (println "Equality test" (= test-vert (p/read-buffer vb 0)))


    ))

;;; }}}
