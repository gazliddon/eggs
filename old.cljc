
(comment 
  (defprotocol IGL
    (clear! [r g b a d])
    (blend-additive! [_])
    (use-program! [_ p])
    (draw-arrays! [_ prim offset n]))

  (extend-type WebGLRenderingContext 
    (clear! [this r g b a d]
      (gl/clear-color-and-depth-buffer gl r g b a d))

    (blend-additive! [gl]
      (.enable gl glc/blend )
      (.blendFunc gl glc/src-alpha glc/one))

    (use-program! [gl p]
      (.useProgram gl p))

    (draw-triangles! [gl offset n]
      (.drawArrays gl glc/triangles 0 n)) )
  )
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
;;;{{{  Players and stuff


(defn draw-printable [ctx printable cam model-mat]
  (gl/draw-with-shader ctx (assoc-in (cam/apply printable cam)
                                     [:uniforms :model] model-mat)))


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
