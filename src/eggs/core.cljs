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


; (defn cb (cub/cuboid))





;;;{{{  Players and stuff
(defn spin
  [t]
  (->
    mat/M44 
    (geom/rotate-y  (/ t 2) )
    (geom/rotate-z  (* t 2) )))

(defn xlate [v] (-> mat/M44 (g/translate v)))

(defn scale [v] (-> mat/M44 (g/scale v)))


(defn draw-printable [ctx printable cam model-mat]
  (gl/draw-with-shader ctx (assoc-in (cam/apply printable cam)
                                     [:uniforms :model] model-mat)))

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

(defn draw-frame! [ctx t pos cam printable]
  (let [ mat (xlate pos) ]
    (doto ctx

      

      ; (draw-printable printable cam (xlate pos))
      
      )))

;; Draw stuff
(defmulti draw-obj (fn [o t ] (:type o)) )

(defmethod draw-obj :player [o t]
  )

;; Update Stuff
(defmulti update-obj (fn [o t inputs] (:type o)) )

(defmethod update-obj :default [_ _ _]
  (println "error"))

(def player-cfg
  {:scale (vec2 0.01 0.01)
   :max-v 0.35
   :fric 0.8 }) 

(defn- clamp [v l]
  (if (> (m/mag v) l )
    (m/* (m/normalize v) l)
    v))

(defn add-pos-vel [{:keys [pos vel] :as o} ]
  (assoc o
         :vel vel
         :pos (+ vel pos)))

(defn reset-pos-vel [o]
  (assoc o :vel (vec2 0 0) :pos (vec2 0 0)))

(defn friction [{:keys [vel] :as o } fr]
  (assoc o :vel (m/* vel fr)))

(defn towards [{:keys [pos vel] :as o} dir fric max-v]
  (let [vel (clamp
                (m/+ dir
                     (m/* vel fric))
                max-v)]
      (assoc o
             :pos (m/+ vel pos)
             :vel vel)))

(defmethod update-obj :player [{:keys [pos vel] :as p} t pad]

  (if (joypads/get-fire-2 pad) 

    (assoc p 
           :vel (vec2 0 0)
           :pos (vec2 0 0))

    (let [joy-dir (joypads/get-left-stick pad)
          dir (m/* (:scale player-cfg) joy-dir)
          v-add (vec2 (/ (Math/cos t) 500 ) 0) ]

      (towards p dir (:fric player-cfg) (:max-v player-cfg)))))


(defn mk-player []
  {:pos (vec2 0 0)
   :vel (vec2 0 0)
   :type :player })


;;;}}}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{ Game Update

(defonce player (atom (mk-player)))

(defn update-game! [t ctx cam pads printable ]
  (let [pad (joypads/poll! pads)]
    (do
      (swap! player update-obj t pad)
      (let [proj (:proj cam)
            view (:view cam) ]
        (pt/update-uniforms! ctx :proj proj :view view)
        (draw-frame! ctx t (:pos @player) cam printable)))))

(def pads (joypads/mk-pads))
(defonce gl-window (glw/mk-gl-window "main"))


(js-log gl-window)
(defonce gl-ctx (:gl gl-window))
(defonce camera (:cam gl-window))
(defonce printable (pt/get-printable :quad))

(defonce stats (stats/mk-stats))

(def timer (timer/mk-timer))

(defn- attach! [obj event-name method]
  (do 
    (.addEventListener 
      (gdom/getWindow)
      event-name 
      (fn [event] (method obj event))))
  obj)


(defn on-js-reload [])





;; }}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; {{{ Some component stuff - todo later when I need a keyboard

(defrecord GameLoop [pad keyboard ctx]
  c/Lifecycle

  (start [this]
    this)

  (stop [this]
    this))

(defn mk-game-loop []
  (map->GameLoop {}))

(defrecord App [running? config]
  c/Lifecycle

  (start [this]
    (if running?
      this
      (assoc this :running? true)))
  
  (stop [this]
    (if running?
      (assoc this :running? false)
      this)))

(defn mk-game [config ]
  (c/system-map
    :keyboard (kb/mk-keyboard)
    :config config
    :pads "pads"
    :ctx "gl"

    :game-loop (c/using 
                 (mk-game-loop)
                 [:pads :keboard :ctx :config])

    :app (c/using
           (map->App {})
           [:game-loop :config ])))

(defonce sys-atom (atom nil))

(defn stop []
  (when @sys-atom
    (c/stop-system @sys-atom)
    (reset! sys-atom nil)) )

(defn start [])
(defn restart [])

;; }}}

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
                      :a_color0 (vec4 1 0 1 0.5)
                      :a_color1 (vec4 0 1 0 0.5)})
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

(defn get-cam-pos [t]
  (let [t (* t 1)
        x (* 5 (Math/cos t ) )
        y 1
        z  -8]
    (vec3 x y z)))



(defn gen-tube []
  (let [rings 10
        len 10]
    )
  )

;; test code
(def gl gl-ctx)

(def shader-ch (async-load-shader gl line-shader-spec) )

(def vb (glvb/mk-vert-buffer gl (:attribs line-shader-spec) (* 80 (count many-lines)))  )

(doseq [[idx v] (map-indexed vector many-lines)]
  (p/write-buffer! vb idx v))

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

(defn update! [gl t shader]
  (stats/begin stats)

  (let [{:keys [aspect]} (glw/update-wh! gl-window)
        t (/ t 3)
        r (cos-01 t 0 3)
        g (cos-01 t 1 1.3)
        b (cos-01 t 2 -0.3) 
        cam-pos (get-cam-pos t)
        cam (cam/perspective-camera {:aspect aspect
                                     :fov 75
                                     :eye cam-pos
                                     :target (vec3 0 0 0)
                                     :near 0.001
                                     :far 1000
                                     }) 

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

    (set-unis! gl shader (get-sp-unis t unis) )
    (.drawArrays gl glc/triangles 0 (count many-lines))

    (set-unis! gl shader (assoc 
                             (get-sp-unis (+ (* t 0.1) 1.5) unis) 
                             :u_hardness (vec2 1)
                             :u_radii (vec2 0.1)
                             :u_inner_color (vec4 0 1 0 0.5)
                             :u_model (xlate (vec3 5 0 0 ))))

    (.drawArrays gl glc/triangles 0 (count many-lines))


    (doseq [i (range 5)]


      (set-unis! gl shader (assoc 
                             (get-sp-unis (+ (* t (+ 3 i)) i) unis) 
                             :u_hardness (vec2 1)
                             ; :u_radii (m/* (vec3 2)  1)
                             :u_inner_color (vec4 1 0 0 0.5)
                             :u_model (xlate (vec3 (+ -5 (* i 5))  0 0 ))))
      (.drawArrays gl glc/triangles 0 (count many-lines))
      )


    (update-game! t gl camera pads printable))
  (stats/end stats))


(go 
  (let [shader (async/<! shader-ch)]

    (use-program! gl shader )

    (p/buffer-data! vb gl)

    (anim/animate (fn [t]
                    (update! gl t shader )))))

;; vim:set fdm=marker : set nospell :
