(ns eggs.timer

  (:require))

(defprotocol ITimer
  (get-time [_]))

(defn get-sys-time-millis []
  (double (.now js/performance)))

(defrecord Timer [base-time]

  ITimer
  (get-time [this]
    (- (get-sys-time-millis) base-time )))

(defn mk-timer []
  (->Timer (get-sys-time-millis)))


