(ns eggs.thrust
  (:require 
    [cljs.pprint :refer [pprint]]
    [util.math :refer [cos sin]]
    [thi.ng.math.core :as m]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

;; Generic
(defn bool->01 
  "1.0 if true 0.0 if false" [b] (if b 1.0 0.0))

(defn bools->twonit
  "true a = -1, true b = 1, true a b 0, false a b 0"
  [a b] (- (bool->01 b) (bool->01 a)))

(def zero-v2 (vec2 0 0 ))

;; physics stuff, verlet and otherwise
;; stolen from https://www.gamedev.net/forums/topic/183827-c64-quotthrustquot-style-physics/

(defn update-phys [{:keys [forces mass vel pos acc] :as this} dt ]
  (let [dt-v2   (vec2 dt)
        mass-v2 (vec2 mass)
        acc     (m/+ acc (m/div forces mass-v2))
        acc-dt  (m/* acc dt-v2)
        pos     (m/+ pos (m/* dt-v2 vel) (m/* acc-dt dt-v2 ))
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
;; TBD

; (defprotocol IPhys
;   (update-phys [this dt])
;   (add-force [this f])
;   (add-acc [this acc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IObj
  (update-obj [this input dt]))

;; constants
(def ship-vals 
  {:thrust-v 4.0
   :grav-v (vec2 0  -9.81 )
   :angle-v 10.0 })

(defrecord Ship [forces acc vel pos angle mass ]
  IObj
  (update-obj [this {:keys [left right fire] :as input} dt ]
    
    (let [{:keys [thrust-v grav-v angle-v] } ship-vals
          rotation (* angle-v (bools->twonit left right))
          angle    (+ angle (* dt rotation)) 
          dir      (vec2 (sin angle) (cos angle)) 
          new-ship (-> this
                       (assoc :acc    (m/+ acc grav-v)
                              :angle  angle
                              :forces (m/+ forces (m/* dir (vec2  (* (bool->01 fire) thrust-v)))))
                       (update-phys dt)) ]
      new-ship
      )))

(defn mk-ship []
  (->
    (map->Ship { :angle  0 }) 
    (init-phys)))



