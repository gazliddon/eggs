(ns eggs.bullet
  (:require
    [thi.ng.math.core :as m :refer [PI HALF_PI TWO_PI]]
    

    )
  )


;; {{{bullets



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

;;{{{

;; }}}



