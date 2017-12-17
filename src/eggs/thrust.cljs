(ns eggs.thrust
  (:require 
    [thi.ng.math.core :as m :refer [PI]]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]  
    [thi.ng.geom.core :as g]
    [thi.ng.geom.matrix :as mat]
    [thi.ng.geom.core :as geom]

    [eggs.fontvb :as font ]
    [eggs.objs :as objs ]

    [cljs.pprint :refer [pprint]]

    [util.vec4 :refer [vec4]]
    [util.math :refer [cos sin]]
    [util.misc :refer [map-keys]]
    ))

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
  
  (draw-obj [this {:keys [font] :as r}]
    (let [ model (-> mat/M44 (g/translate pos) (geom/rotate-z (- 0  (+ PI angle )))) ]
      (do 
        (font/print-it-mat font model (vec4 0.2 0.2 1 0.8) :A) )))

  (get-id [this] id)

  (update-obj [this dt {:keys [left right fire reset]  :as input}]
    (if reset
      (mk-ship id)

      (let [{:keys [thrust-v grav-v angle-v] } ship-vals
            rotation (* angle-v (bools->twonit left right))
            angle    (+ angle (* dt rotation)) 
            dir      (vec2 (sin angle) (cos angle)) 
            thrust   (* thrust-v (bool->01 fire)) 
            forces   (m/+ forces (m/* dir thrust)) ]

        (-> this
            (assoc :acc    (m/+ acc grav-v)
                   :angle  angle
                   :forces forces)

            (update-phys dt))))))

(defn mk-ship [id]
  (->
    (map->Ship { :angle 0 :id id}) 
    (init-phys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn xlate [pos]
  (-> mat/M44 (g/translate pos) )
  )

(defrecord GenericObj [pos id draw frame col]
  objs/IObj

  (get-id [_]
    id)

  (update-obj [this dt input]
    this)

  (draw-obj [this {:keys [font]}]
    (when draw 
      (font/print-it-mat font (xlate pos) col frame))))

(defn mk-particle [id]
  (map->GenericObj {:id id 
                    :pos (vec2 0 0)
                    :draw true 
                    :frame :O 
                    :col (vec4 1 0 0 0.2) }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-link [id id-a id-b]
  (map->GenericObj {:id id :pos (vec2 0 0)}) )

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
           (map-keys #(objs/update-obj % dt input) objs-of-type))
    objs))

(defn update-objs [objs dt input]
  (-> objs
    (update-objs-of-type dt :ships input)
    (update-objs-of-type dt :particles input)))

(defn get-font-u [cam]
  {:u_proj (:proj cam)
   :u_view (:view cam)
   :u_radii (vec2  0.09)
   :u_inner_color (vec4 1 1 1 1)
   :u_outer_color (vec4 1 1 1 1)
   :u_hardness (vec2 0.0000001) } )

(defn draw-objs [objs {:keys [shader cam font] :as r}]
  (do 
    (font/start-text font shader (get-font-u cam))  
    (doseq [[t objs] objs]
      (doseq [[k o] objs]
        (objs/draw-obj o r)))))


