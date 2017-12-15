(ns eggs.thrust
  (:require 
    [thi.ng.math.core :as m]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

(defrecord Ship [forces acc vel pos angle mass])

;; verlet physics stuff

(defn update-phys [{:keys [forces mass vel pos acc] :as physobj} dt ]

  (let [mass-v2 (vec2 mass)
        dt-v2   (vec2 dt)
        acc     (+ acc (m/* forces mass-v2))
        acc-dt  (* acc dt-v2)
        pos     (+ pos (m/* dt-v2 vel) (m/* acc-dt dt-v2 ))
        vel     (+ vel acc-dt ) ]

    (assoc physobj 
           :forces (vec2 0)
           :acc (vec2 0)
           :vel vel
           :pos pos)))

