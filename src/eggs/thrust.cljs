(ns eggs.thrust
  (:require 
    [eggs.objs :as objs ]
    [cljs.pprint :refer [pprint]]
    [util.math :refer [cos sin]]
    [thi.ng.math.core :as m]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

;; {{{ Generic
(defn bool->01 
  "1.0 if true 0.0 if false" [b] (if b 1.0 0.0))

(defn bools->twonit
  "true a = -1, true b = 1, true a b 0, false a b 0"
  [a b] (- (bool->01 b) (bool->01 a)))

(def zero-v2 (vec2 0 0 ))

;; }}}

;; {{{ physics stuff, verlet and otherwise
;; stolen from https://www.gamedev.net/forums/topic/183827-c64-quotthrustquot-style-physics/

(defn update-phys [{:keys [forces mass vel pos acc] :as this} dt ]
  (let [acc     (m/+ acc (m/div forces mass))
        acc-dt  (m/* acc dt)
        pos     (m/+ pos (m/* vel dt) (m/* acc-dt dt ))
        vel     (m/+ vel acc-dt ) ]

    (assoc this 
           :forces zero-v2
           :acc zero-v2
           :vel vel
           :pos pos)))

(defn init-phys [this]
  (assoc this 
         :forces zero-v2
         :acc    zero-v2
         :vel    zero-v2
         :pos    zero-v2
         :mass   0.1))

;; }}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants
(def ship-vals 
  {:thrust-v 4.0
   :grav-v (vec2 0  -9.81 )
   :angle-v 10.0 })


(declare mk-ship)

(defrecord Ship [forces acc vel pos angle mass id]
  objs/IObj
  
  (draw-obj [this r]
    )

  (get-id [this] id)

  (update-obj [this dt {:keys [left right fire] :as input}]

    (if (:reset input)
      (mk-ship id)
      
      (let [{:keys [thrust-v grav-v angle-v] } ship-vals
          rotation (* angle-v (bools->twonit left right))
          angle    (+ angle (* dt rotation)) 
          dir      (vec2 (sin angle) (cos angle)) 
          new-ship (-> this
                       (assoc :acc    (m/+ acc grav-v)
                              :angle  angle
                              :forces (m/+ forces (m/* dir (vec2  (* (bool->01 fire) thrust-v)))))
                       (update-phys dt)) ]
      new-ship))))

(defn mk-ship [id]
  (->
    (map->Ship { :angle 0 :id id}) 
    (init-phys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-particle [id]
  {:id id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-link [id id-a id-b]
  {:id id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-objs [objs]
  (let [ship (mk-ship :ship-o)
        particle (mk-particle :particle-o)
        link (mk-link :link-o :ship-o :particle-o) ]
    (-> objs
        (objs/add-obj :ships ship)
        (objs/add-obj :objs particle)
        (objs/add-obj :links particle))))

(defn update-objs-of-type [objs dt typ input]
  (if-let [objs-of-type (get objs typ)]
    (assoc objs typ
           (map #(objs/update-obj % dt input) objs-of-type))
    objs))

(defn update-objs [objs dt input]
  (-> objs
    (update-objs-of-type dt :ships input)
    (update-objs-of-type dt :particles input)))

(defn draw-objs [objs r]
  (doseq [[t objs] objs]
    (doseq [o objs]
      (objs/draw-obj o r))))

(comment 
  (let [input @keys-atom
        dt (/ 1.0 60.0)
        new-ship (if (:reset @keys-atom)
                   (mk-ship) 
                   (thrust/update-obj @ship input dt)) ]
    (do 
      (reset! ship new-ship)
      (draw-ship new-ship font-printer shader (get-ship-cam aspect 50))))

  (def ship (atom (mk-ship) ))

  (defn get-ship-cam [aspect w]
    {:view (scale (vec3 (/ 1 w) (/ 1 w) 1))
     :proj (scale (vec3 1.0 aspect 1.0))})

  (defn draw-ship [{:keys [pos angle]} font shader cam]
    (do 
      (let [ model (-> mat/M44 (g/translate pos) (geom/rotate-z (- 0  (+ PI angle )))) ]
        (font/start-text font shader {:u_proj (:proj cam)
                                      :u_view (:view cam)
                                      :u_radii (vec2  0.09)
                                      :u_inner_color (vec4 1 1 1 1)
                                      :u_outer_color (vec4 1 1 1 1)
                                      :u_hardness (vec2 0.0000001) })

        (font/print-it-mat font model (vec4 0.2 0.2 1 0.8) :A))))

  (def objs-a (atom (objs/mk-objs)))

  (defn ship-it  []
    (let [ship (mk-ship)
          blob (mk-blob) ]
      (-> @objs-a
          (add-object :ship ship)
          (add-object :blob blob)
          (add-object :link (mk-link (objs/get-id ship)
                                     (objs/get-id blob)))

          )
      )
    )


  )


