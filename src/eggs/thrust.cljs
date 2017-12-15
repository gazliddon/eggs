(ns eggs.thrust
  (:require 
    [util.math :refer [cos sin]]
    [thi.ng.math.core :as m]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

;; Generic

(defn bool->01
  "1.0 if true 0.0 if false"
  [b] (if b 1.0 0.0))

(defn bools->twonit
  "true a = -1, true b = 1, true a b 0, false a b 0"
  [a b] (- (bool->01 b) (bool->01 a)))

(def zero-v2 (vec2 0 0 ))

;; physics stuff, verlet and otherwise
;; stolen from https://www.gamedev.net/forums/topic/183827-c64-quotthrustquot-style-physics/

(defn update-phys [{:keys [forces mass vel pos acc] :as this} dt ]
  (let [acc     (m/+ acc (m/* forces mass))
        acc-dt  (m/* acc dt)
        pos     (m/+ pos (m/* dt vel) (m/* acc-dt dt ))
        vel     (m/+ vel acc-dt ) ]

    (assoc this 
           :forces zero-v2
           :acc zero-v2
           :vel vel
           :pos pos)))

(defrecord Ship [forces acc vel pos angle mass angle])

;; constants
(def ship-vals 
  {:thrust-v 1.0
   :grav-v (vec2 0  -9.81 )
   :angle-v 1.0 })

(defn update-ship [{:keys [angle forces acc] :as this} dt {:keys [left right fire] :as pad} ]
  (let [{:keys [thrust-v grav-v angle-v] } ship-vals
        rotation (* angle-v (bools->twonit left right))
        angle    (+ angle (* dt rotation)) 
        dir      (vec2 (cos angle) (sin angle)) ]
    (-> this
        (assoc :acc    (m/+ acc grav-v)
               :angle  angle
               :forces (m/+ forces (m/* dir (* (bool->01 fire) thrust-v))))
        (update-phys dt))))

(defn mk-ship []
  (map->Ship 
    {:forces (vec2 0)
     :acc (vec2 0)
     :vel (vec2 0)
     :pos (vec2 0)
     :mass 1
     :angle 0 }))



