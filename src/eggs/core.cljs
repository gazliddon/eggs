(ns eggs.core

  ; {{{ Requires

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] 
    [thi.ng.math.macros :as mm]
    [eggs.macros :refer [with-vb]])

  (:require

    [figwheel.client :as fwc]
    [thi.ng.geom.cuboid :refer [cuboid]]
    [thi.ng.geom.sphere :refer [sphere]]

    [util.stats :as stats]
    [util.math :refer [cos sin cos-01 map-range]]

    [thi.ng.xerror.core :as err]
    [thi.ng.geom.gl.webgl.constants :as glc]

    [eggs.lineshader :refer [line-shader-spec]]

    [eggs.shaders :refer [async-load-shader]]

    [eggs.resources :as res]
    [eggs.glvertbuffer :as glvb :refer [mk-vert-buffer!]]
    [eggs.protocols :as p]
    [eggs.glwindow :as glw]
    [eggs.printables :as pt]

    [util.vec4 :refer [vec4]]
    [util.misc :refer [js-log map-kv]]

    [cljs.core.async :as async :refer [chan] ]
    [cljs.pprint :refer [pprint]]

    [goog.dom :as gdom] 
    [goog.events :as gev]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf ]]

    [cljs.spec.alpha :as s ]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]

    [eggs.glhelpers :refer 
     [gl-clear! set-uni! set-unis! use-program!] ]

    [thi.ng.geom.gl.core :as gl]
    [thi.ng.geom.matrix :as mat]
    [thi.ng.geom.triangle :as tri] 
    [thi.ng.geom.quad :as quad] 
    [thi.ng.geom.core :as geom]
    [thi.ng.geom.gl.glmesh :as glmesh]
    [thi.ng.geom.gl.camera :as cam]
    [thi.ng.geom.gl.shaders :as shaders] 
    [thi.ng.geom.gl.webgl.animator :as anim])
  
  ;; }}}
  )

(enable-console-print!)

;; {{{ Helpers

(defn spin
  [t]
  (->
    mat/M44 
    (geom/rotate-y  (/ t 2) )
    (geom/rotate-z  (* t 2) )))

(defn xlate [v] (-> mat/M44 (g/translate v)))
(defn scale [v] (-> mat/M44 (g/scale v)))

;; }}}

;;{{{ Stuff for making lines

(defprotocol ILines
  (set-base [this new-base])
  (add-line-v [this new-v])
  (add-line-p [this p0 p1]))

(defprotocol IVerts
  (get-verts [this]))

(defrecord Verts [base verts]
  ILines
  (set-base [this new-base]
    (assoc this :base new-base))

  (add-line-v [this new-v]
   (let [new-base (merge base new-v)
         new-verts (map #(assoc new-base :a_index %) [1 0 3 0 3 2])]
      (assoc this :verts (into verts new-verts) :base new-base )) )

  (add-line-p [this p0 p1]
    (add-line-v this {:a_position0 p0 :a_position1 p1})))

(defn mk-line-verts [ base-hash ]
  (map->Verts {:base base-hash :verts []}))

(defn add-lines [verts lines]
  (loop [verts verts lines lines ]
    (if (> (count lines) 1)
      (recur
        (add-line-p verts (first lines) (second lines))
        (rest lines))
      verts)))

;; }}}

;; {{{ Cameras

(defn mk-current-cam [cam-defaults cam-fn t]
 (let [cam (-> (merge cam-defaults (cam-fn t))
                (cam/perspective-camera )) ]
   cam))

(defn get-vp [cam-defaults cam-fn t]
  (let [cam (mk-current-cam cam-defaults cam-fn t) ]
    {:u_proj (:proj cam)
     :u_view (:view cam) }))

(defn fmod [a b]
  (- a (Math/floor (* (/ a b) b ))))

(defn get-cam-pos-1 [t]
  (let [t (* t 1)
        x (* 5 (Math/cos t ) )
        y 1
        z  -8]
    {:eye (vec3 x y z) }))

(defn no-cam [t]
  {:eye (vec3 0 0 -2)
   :target (vec3 0 0 0)
   :fov 70 })

(def cams 
  {:cam-1 (fn [t]
            (let [t (fmod t 20000)
                  x (- (* 10 t) 30)  
                  y 3
                  z 1]

              {:eye (vec3 x y z)
               :fov 30
               :target (vec3 0)}))

   :cam-2 get-cam-pos-1 

   :no-cam no-cam })

(def c-cam (atom (keys cams)))

(defn get-current-cam []
  (get cams (first @c-cam)))

(defn rotate-v [v]
 (into (vec (rest v)) [ (first v) ]) )

(defn next-cam! []
  (swap! c-cam rotate-v))

(defn set-cam! [cam-key]
  (if (contains? cams cam-key )
    (while (not= (first @c-cam) cam-key)
      (next-cam!))
    (t/warn (str "couldn't find cam " cam-key " in " @c-cam)) 
    )
  )

(defn key->cam! [ch]
  (let [mapping {\1 :cam-1
                 \2 :cam-2 
                 \3 :no-cam
                 \4 :no-cam
                 \5 :no-cam
                 \6 :no-cam
                 \7 :no-cam
                 \8 :no-cam
                 \9 :no-cam
                 \0 :no-cam } ]
    (when-let [cam (get mapping ch)] (set-cam! cam))))
;; }}}

;; {{{ Helpers
(defn draw-vb-tris! [gl vb shader unis]
  (do 
    (use-program! gl shader)
    (p/make-active! vb gl shader)
    (set-unis! gl shader unis)
    (.drawArrays gl glc/triangles 0 (:num-of-verts vb))))

;;; }}}

;; {{{ Stars
(defn get-vert [i f]
  (let [ic (/ i 3.0)
        z 0
        y ( * 2  (Math/cos (* f 10)))   
        y2 ( * 2  (Math/cos (* (+ f 0.01) 10)))   
        x (map-range f -160 160) 
        r (cos-01 ic 1 3)
        g (cos-01 ic 3 3)
        b (cos-01 ic 1 3) 
        sc 4
        ]
  {:a_position0 (vec3 x y z)
   :a_position1 (vec3 (+ 2 x) y2 z)
   :a_color0 (vec4 (* sc r )(* sc g )(* sc b ) 0.7)
   :a_color1 (vec4 r g b 0) }))

(defn make-stars [gl steps]
  (let [verts (mk-line-verts {:a_radii (vec2 0.9 0.01)}) 
        verts (-> (fn [acc i]
                    (let [f (/ i steps )
                          v (get-vert i f) ]
                      (add-line-v acc v)))
                  (reduce verts (range steps))) ]

        (mk-vert-buffer! gl (:attribs line-shader-spec) (:verts verts))))

(defn draw-stars! [gl t vb shader unis]
  (let [unis (assoc 
               unis 
               :u_hardness (vec2 0.0009 0.00000001)
               :u_radii (vec2 0.7 0.0001)
               :u_inner_color (vec4 2 2 2 1)
               :u_outer_color (vec4 -1 -1 -1 2))]
    (draw-vb-tris! gl vb shader unis )))

;; }}}

;; {{{ events hacks
(defn handle-key [ev]
  (let [k (char  (first  (.-key ev)))]
    (key->cam! k)))

(defn handle-click [ev]
  (next-cam!))

(defn single-listen! [o ev-type f]
  (gev/removeAll o ev-type)
  (gev/listen o ev-type f))

(defn init! []
  (single-listen! js/window gev/EventType.KEYPRESS handle-key)
  (single-listen! js/window gev/EventType.CLICK handle-click))

;;; }}}

;; {{{ Sphere obj 
(defn mk-sphere! [gl ]
  (let [attribs (:attribs line-shader-spec)
        sp (thi.ng.geom.sphere/sphere 1.8)
        cc (map #(g/random-point sp) (range 200))
        mm (g/as-mesh sp {:res 25})

        many-lines (-> (mk-line-verts {:a_radii  (vec2 0.2 0.7)
                                       :a_color0 (vec4 1 0 1 0.2)
                                       :a_color1 (vec4 0 1 0 0.2)})
                       (add-lines (:vertices mm))
                       :verts) ]

    (mk-vert-buffer! gl attribs many-lines)))

(defn get-sp-unis [t]
  (let [r (cos-01 t 0 3)
        g (cos-01 t 1 1.3)
        b (cos-01 t 2 -0.3) ]
    {:u_hardness (vec2 b)
     :u_outer_color (vec4 b r g (+ 0.5 g))  
     :u_inner_color (vec4 1 1 g r)
     :u_radii (vec2 (* 1.9 (Math/cos t) )  (* 2.0 r)) }))

;; }}}

;; {{{ Helpers hex printer
(def n-hex-char "0123456789abcdef")
(defn get-hex-ch [n] (nth n-hex-char (bit-and 0xf n)))
(defn hex-str [i]
  (if (= 0 i)
    "0x0"
    (loop [ret "" i (int i)]
      (if (pos? i)
        (recur 
          (.concat (get-hex-ch i) ret)
          (bit-shift-right i 4))
        (.concat "0x" ret)))) )

(defn hex-array-str [data]
  (str  (mapv hex-str data)))
;; }}}

;; {{{ Midi!

(defn get-access []
  (let [ch (async/chan)]
    (doto (.requestMIDIAccess js/navigator)
      (.then (fn [a] 
               (go (>! ch a) 
                   (t/info "Midi access granted")
                   (async/close! ch))) 
             (fn [err] (go 
                         (t/warn "Midi access refused")
                         (>! ch :failed)
                         (async/close! ch)))))
    ch))

(defn midi-seq [acc nm]
  (let [seqq (aget acc (name nm))]
    (map (fn [[n o]] o) (es6-iterator-seq (-> seqq .entries)))))

(defn midi-hash [acc nm]
  (let [seqq (midi-seq acc nm)]
    (-> 
      (fn [acc o] (assoc acc (.-name o) o))
      (reduce {} seqq))))


(defn add-note [{:keys [data] :as inf } ]
  (assoc inf :note (aget data 1)))

(defn add-channel [{:keys [data] :as inf } ]
  (assoc inf :chan (bit-and 0xf (aget data 0))))

(defn add-velocity [{:keys [data] :as inf } ]
  (assoc inf :velocity (aget data 2)) )

(defn add-chan-note-vel [m]
 (-> m (add-note) (add-channel) (add-velocity)) )

(defn midi-inf [data type ]
  {:type type 
   :data data })

(defn is-data? [data type lo hi]
  (let [n (aget data 0)]
    (when (and (>= n lo) (<= n hi))
        (midi-inf data type ))))

(def handlers 
  {:note-down {:range [0x90 0x9f]
               :handler (fn [m _] (add-chan-note-vel m))}

   :note-off  {:range [0x80 0x8f]
               :handler (fn [m _] (add-chan-note-vel m)) }

   :cc-msg    {:range [0xb0 0xbf]
               :handler (fn [m data] (assoc m :cc-num (aget data 1))) } })

(def handlers-tab
  [ (fn [data]
     (when-let [msg (is-data? data :note-down 0x90 0x9f)] 
       (-> msg (add-note) (add-channel) (add-velocity))))

   (fn [data]
     (when-let [msg (is-data? data :note-off 0x80 0x8f)]
       (-> msg (add-note) (add-channel) (add-velocity))))

   (fn [data]
     (when-let [msg (is-data? data :cc 0xb0 0xbf)] 
       (-> msg (add-channel) (assoc :cc-num (aget data 1)
                                    :val    (aget data 2))))) ])

(defn parse-midi [data]
  (->
    (fn [acc v]
      (if acc acc (v data)))
    (reduce nil handlers-tab)))

(def cc->val (atom {}))

(pprint cc->val)


(defn on-midi-in [n]
  (let [time-stamp (.-timeStamp n)
        data  (.-data n) 
        parsed (parse-midi data) ]
    (when parsed 
      (cond 
        (= (:type parsed) :cc) (do 
                            (swap! cc->val assoc (:cc-num parsed) (:val parsed)))
        :else nil))))

(defn on-midi-chan-state-change [n])

(defonce vv
  (go 
    (def access (<! (get-access)))
    (t/info "****  Starting MIDI")
    (def first-in (first (midi-seq access :inputs)))
    (when first-in 
      (t/info (str "Attaching to midi in " (.-name first-in)))
      (aset first-in "onmidimessage" on-midi-in)
      (aset first-in "onstatechange" on-midi-chan-state-change)
      (js-log first-in)
      (t/info "****  Initialised MIDI"))
    :hello))
;;}}}


;; test code

(defonce gl-window (glw/mk-gl-window "main"))
(defonce gl (:gl gl-window))
(defonce stats (stats/mk-stats))

(def vb  (mk-sphere! gl) )

(def stars-vb (make-stars gl 20))

(def cam-defaults {:fov 75
                   :eye (vec3 0 2 0)
                   :target (vec3 0 0 0)
                   :near 0.001
                   :far 1000 })

(defn update! [gl t shader]
  (stats/begin stats)

  (gl-clear! gl 0 0 0.1)
  (.enable gl glc/blend )
  (.blendFunc gl glc/src-alpha glc/one)

  (let [{:keys [aspect]} (glw/update-wh! gl-window)

        cam-defaults (assoc cam-defaults :aspect aspect)

        t (/ t 3)
        r (cos-01 t 0 3)
        g (cos-01 t 1 1.3)
        b (cos-01 t 2 -0.3) 

        unis  (merge 
                (get-vp cam-defaults (get-current-cam) t ) 
                {:u_model mat/M44
                 :u_hardness (vec2 (/ b 3))
                 :u_outer_color (vec4 b r g (/ g 0.5))  
                 :u_inner_color (vec4 1 0 g (* 0.5  (- 1.0 g)))
                 :u_radii (vec2 (* 1.9 (Math/cos t) )  (* 2.0 r)) }) ]

    (if-let [cc-val (get @cc->val 64)]
      (doseq [i (range 10)]
        (let [pos (vec3 (+ -5 (* i 5)) (Math/cos (+ (/ cc-val 12 ) t)) 0 )
              unis (-> unis
                       (merge (get-sp-unis (+ (* t (+ 3 i)) i) ))
                       (assoc :u_model (xlate pos)))]

          (draw-vb-tris! gl vb shader unis) ))  )
    

    (draw-stars! gl t stars-vb shader unis))

  (stats/end stats))

(go 
  (init!)
  (def shader-ch (async-load-shader gl line-shader-spec) )

  (let [shader (async/<! shader-ch)]
    (defonce doit 
      (anim/animate (fn [t]
                      (update! gl t shader ))))))

;; vim:set fdm=marker : set nospell :
