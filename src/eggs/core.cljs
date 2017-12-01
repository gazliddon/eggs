(ns eggs.core
  ; {{{ Requires

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] 
    [thi.ng.math.macros :as mm])

  (:require
    [thi.ng.geom.cuboid :refer [cuboid]]
    [thi.ng.geom.sphere :refer [sphere]]
    [util.stats :as stats]
    [thi.ng.xerror.core :as err]
    [thi.ng.geom.gl.webgl.constants :as glc]

    [eggs.lineshader :refer [async-load-shader line-shader-spec]]
    [eggs.resources :as res]
    [eggs.glvertbuffer :as glvb]
    [eggs.protocols :as p]
    [eggs.glwindow :as glw]
    [eggs.timer :as timer]
    [eggs.keyboard :as kb]
    [eggs.printables :as pt]
    [eggs.pad :as joypads]

    [util.vec :as V]
    [util.vec4 :refer [vec4]]
    [util.misc :refer [js-log map-kv]]

    [cljs.core.async :as async ]
    [cljs.pprint :refer [pprint]]

    [com.stuartsierra.component :as c]
    [goog.dom :as gdom] 
    [goog.events :as gev]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf ]]



    [cljs.spec.alpha :as s ]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.types]
    [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]

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

(defonce gl-window (glw/mk-gl-window "main"))


(js-log gl-window)
(defonce gl-ctx (:gl gl-window))
(defonce camera (:cam gl-window))
(defonce printable (pt/get-printable :quad))

(defonce stats (stats/mk-stats))
(defn cos-01 [t phase speed]
  (let [t (+ phase (* speed t) ) ]
    (/ (+ 1.0 (Math/cos t)) 2.0)))

(defn gl-clear! 
  ([gl r g b a d]
   (gl/clear-color-and-depth-buffer gl r g b a d))

  ([gl r g b a]
   (gl-clear! gl r g b a 1))

  ([gl r g b]
    (gl-clear! gl r g b 1 1)))
(defn spin
  [t]
  (->
    mat/M44 
    (geom/rotate-y  (/ t 2) )
    (geom/rotate-z  (* t 2) )))

(defn xlate [v] (-> mat/M44 (g/translate v)))

(defn scale [v] (-> mat/M44 (g/scale v)))

;;{{{ geom for one line

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

(def sq [(vec3 0 0) (vec3 1 0) (vec3 1 1) (vec3 0 1) (vec3 0 0) ])

(defn add-lines [verts lines]
  (loop [verts verts lines lines ]
    (if (> (count lines) 1)
      (recur
        (add-line-p verts (first lines) (second lines))
        (rest lines))
      verts)))


(def sp (thi.ng.geom.sphere/sphere 1.8))
(def cc (map #(g/random-point sp) (range 200)))

(def mm (g/as-mesh sp {:res 25}))

(def many-lines
  (-> (mk-line-verts {:a_radii  (vec2 0.2 0.7)
                      :a_color0 (vec4 1 0 1 0.2)
                      :a_color1 (vec4 0 1 0 0.2)})
      (add-lines (:vertices mm))
      :verts))

;;}}}

(defn set-uni! [gl {:keys [uniforms]} k v]
  (if-let [uni (get uniforms k) ]
    (do 
      ((:setter uni) v))
    (t/warn (str "unknown uniform " k " val " v))))

(defn set-unis! [gl {:keys [uniforms] :as shader} hsh] 
  (doseq [[k v] hsh]
    (set-uni! gl shader k v)))

(defn use-program! [gl {:keys [program] :as shader } ]
  (.useProgram gl program))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn map-range [t mmin mmax]
  (let [r (- mmax mmin) ]
   (+ mmin (* t r)) ))

(defn get-vert [i f]
  (let [ic (/ i 3.0)
        z 0
        y ( * 2  (Math/cos (* f 10)))   
        x (map-range f -150 150) 
        r (cos-01 ic 1 10)
        g (cos-01 ic 3 3)
        b (cos-01 ic 1 2) ]
  {:a_position0 (vec3 x y z)
   :a_position1 (vec3 (+ 0.4 x) y z)
   :a_color0 (vec4 r g b 1)    
   :a_color1 (vec4 r g b 0.001) }))

(defn make-stars [gl steps]
  (let [verts (mk-line-verts {:a_radii (vec2 5.9 0.001)}) 
        verts (-> (fn [acc i]
                    (let [f (/ i steps )
                          v (get-vert i f) ]
                      (add-line-v acc v)))
                  (reduce verts (range steps))) 
        v-data (:verts verts)
        vb (glvb/mk-vert-buffer gl (:attribs line-shader-spec) (* 80 (count v-data))) ]
    (do 
      (doseq [[idx v] (map-indexed vector v-data)]
        (p/write-buffer! vb idx v))
      (p/buffer-data! vb gl)
      vb)))

(defn draw-stars! [gl t vb shader unis]
  (let [unis (assoc 
               unis 
               :u_hardness (vec2 0.9 0.0001)
               :u_radii (vec2 1)
               :u_inner_color (vec4 1 1 1 1)
               :u_outer_color (vec4 1 1 1 1))]
    (do 
      (use-program! gl shader)
      (p/make-active! vb gl shader)
      (set-unis! gl shader unis)
      (.drawArrays gl glc/triangles 0 (:num-of-verts vb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test code
(def gl gl-ctx)

(def shader-ch (async-load-shader gl line-shader-spec) )

(def vb (glvb/mk-vert-buffer gl (:attribs line-shader-spec) (* 80 (count many-lines)))  )
(doseq [[idx v] (map-indexed vector many-lines)]
  (p/write-buffer! vb idx v))
(p/buffer-data! vb gl)

(defn get-sp-unis [t unis]
  (let [r (cos-01 t 0 3)
        g (cos-01 t 1 1.3)
        b (cos-01 t 2 -0.3) ]
    (assoc 
      unis
      :u_hardness (vec2 b)
      :u_outer_color (vec4 b r g (+ 0.5 g))  
      :u_inner_color (vec4 1 1 g r)
      :u_radii (vec2 (* 1.9 (Math/cos t) )  (* 2.0 r)) )))

(def cam-defaults {:fov 75
                   :eye (vec3 0 2 0)
                   :target (vec3 0 0 0)
                   :near 0.001
                   :far 1000 })

(def stars-vb (make-stars gl 100))

(defn update! [gl t shader]

  (stats/begin stats)

  (let [{:keys [aspect]} (glw/update-wh! gl-window)
        cam-defaults (assoc cam-defaults :aspect aspect)
        t (/ t 3)
        r (cos-01 t 0 3)
        g (cos-01 t 1 1.3)
        b (cos-01 t 2 -0.3) 
        cam-fn (get-current-cam)
        cam (cam/perspective-camera (merge cam-defaults (cam-fn t))) 

        unis  {:u_proj (:proj cam)
               :u_view (:view cam)
               :u_model mat/M44
               :u_hardness (vec2 b)
               :u_outer_color (vec4 b r g (/ g 0.5))  
               :u_inner_color (vec4 1 0 g (* 0.5  (- 1.0 g)))
               :u_radii (vec2 (* 1.9 (Math/cos t) )  (* 2.0 r)) }

        ]


    (gl-clear!  gl 0 0 0.1)
    (.enable gl glc/blend )
    (.blendFunc gl glc/src-alpha glc/one)

    (use-program! gl shader)
    (p/make-active! vb gl shader)
    (doseq [i (range 10)]
      (set-unis! gl shader (assoc 
                             (get-sp-unis (+ (* t (+ 3 i)) i) unis) 
                             :u_hardness (vec2 1)
                             ; :u_radii (m/* (vec3 2)  1)
                             ; :u_inner_color (vec4 1 0 0 0.5)
                             :u_model (xlate (vec3 (+ -5 (* i 5)) (Math/cos (+ i t)) 0 ))))
      (.drawArrays gl glc/triangles 0 (count many-lines)))


    (draw-stars! gl t stars-vb shader unis))
  (stats/end stats))

  (defn handle-key [ev]
    (let [k (char  (first  (.-key ev)))]
      (key->cam! k)))

  (defn handle-click [ev]
    (next-cam!))

  (defn single-listen! [o ev-type f]
    (gev/removeAll o ev-type)
    (gev/listen o ev-type f))

(go 

  (let [shader (async/<! shader-ch)]

    (single-listen! js/window gev/EventType.KEYPRESS handle-key)
    (single-listen! js/window gev/EventType.CLICK handle-click)

    ; (use-program! gl shader)

    (defonce doit 
      (anim/animate (fn [t]
                      (update! gl t shader ))))))

;; vim:set fdm=marker : set nospell :
