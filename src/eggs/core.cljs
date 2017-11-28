(ns eggs.core
  ; {{{ Requires

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] 
    [thi.ng.math.macros :as mm])

  (:require
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

;;;{{{  Players and stuff
(defn spin
  [t]
  (->
    mat/M44 
    (geom/rotate-y  (/ t 2) )
    (geom/rotate-z  (* t 2) )))

(defn xlate [v]
  (let [m mat/M44]
    (-> mat/M44
    (g/translate v))))

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

      

      (draw-printable printable cam (xlate pos)))))

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
(defonce gl-ctx (:ctx gl-window))
(defonce camera (:cam gl-window))
(defonce printable (pt/get-printable :quad))

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

(def a {:a_index      0
        :a_position0  (vec2 0 0)
        :a_position1  (vec2 1 1)
        :a_radii      (vec2 0.1 0.1)
        :a_color0     (vec4 0 1 0 1)
        :a_color1     (vec4 1 0 0 1)} )

(def b {:a_index      1
        :a_position0  (vec2 0 0)
        :a_position1  (vec2 1 1)
        :a_radii      (vec2 0.1 0.1)
        :a_color0     (vec4 0 1 0 1)
        :a_color1     (vec4 1 0 0 1)} )

(def c {:a_index      2
        :a_position0  (vec2 0 0)
        :a_position1  (vec2 1 1)
        :a_radii      (vec2 0.1 0.1)
        :a_color0     (vec4 0 1 0 1)
        :a_color1     (vec4 1 0 0 1)} )

(def d {:a_index      3
        :a_position0  (vec2 0 0)
        :a_position1  (vec2 1 1)
        :a_radii      (vec2 0.1 0.1)
        :a_color0     (vec4 0 1 0 1)
        :a_color1     (vec4 1 0 0 1)} )

(def one-line [ a b c b c d ])  

;;}}}

;; test code
(def gl gl-ctx)

(defn use-program! [gl {:keys [program] :as shader } ]
  (.useProgram gl program))

(defn set-uni! [gl {:keys [uniforms]} k v]
  (if-let [uni (get uniforms k) ]
    (do 
      ((:setter uni) v))
    (t/warn (str "unknown uniform " v))))

(defn set-unis! [gl {:keys [uniforms] :as shader} hsh] 
  (doseq [[k v] hsh]
    (set-uni! gl shader k v)))

(def shader-ch (async-load-shader gl line-shader-spec) )
(def vb (glvb/mk-vert-buffer gl (:attribs line-shader-spec) 100)  )

(def unis {:u_vp           mat/M44 
           :u_model        mat/M44 
           :u_hardness     (vec2 0.1 0.1)
           :u_radii        (vec2 1.1 1.1)
           :u_inner_color  (vec4 1 1 1 1)
           :u_outer_color  (vec4 1 1 1 1)
           :u_dist_mul     1.0
           :uv_mul         (vec2 1 1) })

(go 
  (let [shader (async/<! shader-ch)]

    (use-program! gl shader )
    (set-uni! gl shader :u_vp mat/M44)

    ;; set the buffer
    (doseq [[idx v] (map-indexed vector one-line)]
      (p/write-buffer! vb idx v))

    (p/buffer-data! vb gl)

    (anim/animate (fn [t]
                    (gl-clear!
                      gl
                      (cos-01 t 0 3) 
                      (cos-01 t 1 1.3) 
                      (cos-01 t 2 -0.5))

                    (use-program! gl shader )
                    (p/make-active! vb gl shader)
                    (set-unis! gl shader unis)
                    (.drawArrays gl glc/triangles 0 6)

                    (update-game! t gl camera pads printable)



                    )

                  )))

;; vim:set fdm=marker : set nospell :
