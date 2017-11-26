(ns eggs.core
  ; {{{ Requires

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] 
    [thi.ng.math.macros :as mm])

  (:require
   [thi.ng.xerror.core :as err]

    [eggs.resources :as res]
    [eggs.fetch :refer [fetch-files-in-hash]]
    [eggs.vdef :as vdef]
    [eggs.protocols :as p]

    [cljs.core.async :as async ]

    [com.stuartsierra.component :as c]

    [eggs.glwindow :as glw]

    [cljs.pprint :refer [pprint]]

    [goog.dom :as gdom] 

    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf ]]

    [eggs.timer :as timer]
    [eggs.keyboard :as kb]
    [eggs.vdef :as vdef]
    [eggs.printables :as pt]

    [util.vec :as V]
    [util.vec4 :refer [vec4]]
    [util.misc :refer [js-log map-kv]]

    [eggs.pad :as joypads]

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
    [thi.ng.geom.gl.webgl.constants :as glc]
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

      (gl-clear!
        (cos-01 t 0 3) 
        (cos-01 t 1 1.3) 
        (cos-01 t 2 -0.5))

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
;; {{{bullets

(defmethod update-obj :bulllet [{:keys [pos vel] :as b} t pad]
  )

(defprotocol IPredictable
  (get-acc [_ t])
  (get-pos [_ t])
  (get-vel [_ t]))

(defprotocol IDraw
  (draw [_ ctx t]))

(defprotocol IAgeable 
  (get-age [_ t]))


(defrecord Bullet [created-at pos vel type]
  IDraw
  (draw [this ctx t]
    (let [pos (get-pos this t)]
      )
    )

  IAgeable
  (get-age [_ t]
    (- t created-at))

  IPredictable
  (get-acc [this t]
    0)

  (get-vel [this t]
    vel)

  (get-pos [this t]
    (let [age (get-age this t)]
      (m/+ pos 
           (m/* vel age)))))


(defn mk-bullet [created-at pos vel]
  (->Bullet created-at pos vel :bullet))

(defonce bullets (atom []))

(defn add-bullet! [t pos vel]
  (swap! bullets conj (mk-bullet t pos vel)))

(defn filter-bullets! [t max-life]
  (->
    (fn [o]
      (let [age (- t (:time o))]
        (<= age max-life)))

    (filter @bullets)
    (reset! bullets)))

;; }}}

(def triangle (geom/as-mesh (tri/triangle3 [[0.1 0 0] [-0.1 0 0] [0 0.1 0]])
                            {:mesh (glmesh/gl-mesh 3)}))

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

;; {{{ Shader loading
(defn async-load-shader [ shader ]
  (go
    (let [kz [:vs-file :fs-file :common]
          loaded (-> (select-keys shader kz)
                     (fetch-files-in-hash)
                     (async/<!)) 
          {:keys [common vs-file fs-file]} loaded 

          shader (assoc  shader
                        :vs (str common "\r\n" vs-file)
                        :fs (str common "\r\n" fs-file)
                        :vs-file nil
                        :fs-file nil
                        :common nil) ]
      (shaders/make-shader-from-spec gl-ctx shader )
      )))
;; }}}

;;; {{{ Line Shader def

(def line-shader-spec
  {:vs-file "shaders/line.vs"

   :fs-file "shaders/line.fs"

   :common "shaders/common.glsl"

   :version 300

   :varying {:v_uv :vec2
             :v_color :vec4
             :v_radius :float 
             :v_hardness :float}

   :uniforms {:u_vp           :mat4
              :u_model        :mat4
              :u_hardness     [:vec2 [1.0 1.0]]
              :u_radii        :vec2
              :u_inner_color  :vec4
              :u_outer_color  :vec4
              :u_dist_mul     [:float 1.0]
              :uv_mul         :vec2 }

   :attribs  {:a_index      :int
              :a_position0  :vec2
              :a_position1  :vec2
              :a_radii      :vec2
              :a_color0     :vec4 
              :a_color1     :vec4 }})
;;; }}}

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

(def one-line [ a b c b d c ])  

;;}}}

;; {{{ Stolen Attribute enabling / disabling

(defn make-gl-vert-buffer! [{:keys [attr-specs] :as vert-buffer } gl {:keys [attrs] :as shader }]
  (let [gl-buffer (.createBuffer gl)]
    (assoc vert-buffer
           ;; add a gl buffer
           :gl-buffer gl-buffer
           ;; buff attr specs w loc and buffer info
           :attr-specs (-> (fn [k v]
                             (assoc v :gl-buffer gl-buffer
                                    :loc (get attrs k :attr-not-found)))
                           (map-kv attr-specs)))))

(defprotocol IGLVertBuffer 
  (make-active!  [this gl]))

(extend-type vdef/VertBuffer 
  IGLVertBuffer

  (make-active! [{:keys [gl-buffer attr-specs] :as this} gl]
    (do
      (.bindBuffer gl glc/array-buffer gl-buffer)

      (doseq [[id attr-spec] attr-specs]
        (let [{:keys [gl-buffer gl-vert-attr-ptr buffer stride size type normalized? offset loc] } attr-spec] 
          (doto gl
            (.enableVertexAttribArray loc)
            (gl-vert-attr-ptr loc size type normalized? stride offset)))))))
;; }}}

(def shader-ch (async-load-shader line-shader-spec) )


(defn doseq-idx [fun col]
 (doseq [[idx v] (map-indexed vector col)]
   (fun idx v)))

(go 
  (let [gl gl-ctx
        shader (async/<! shader-ch) 
        vb (-> (vdef/mk-vert-buffer (:attribs line-shader-spec) 100) 
               (make-gl-vert-buffer! gl shader)) ]

    ;; set the buffer
    (doseq-idx #(p/write-buffer! vb % %2) one-line)

    ; (make-active! vb gl)

    (anim/animate (fn [t]
                    (update-game! t gl camera pads printable)))

    (t/info "shader loaded")  
    ))

;;}}}

;; vim:set fdm=marker : set nospell :
