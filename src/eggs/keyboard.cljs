(ns eggs.keyboard
  (:require
    [com.stuartsierra.component :as c]
    [eggs.protocols :as pp]))

(defn- bind-key-events [& korks]
  true)

(defn- unbind-key-events [keyz]
  nil)

(defn- on-key-down! [key-states-atom ev]
  )

(defn- on-key-up! [key-states-atom ev]
  )

(defn flush-keys! [key-states-atom]
  (reset! key-states-atom {}))

(defrecord Keyboard [running? key-states-atom events]
  pp/IKeys

  (flush-keys! [this] (flush-keys! key-states-atom))
  (get-key [this k] (get @key-states-atom k {:state false}))

  c/Lifecycle
  (start [this]
    (if running?
      this
      (let [key-states-atom (atom {})]
        (assoc this
               :running? true
               :key-states key-states-atom
               :events (bind-key-events 
                         :up   #(on-key-down! key-states-atom %)
                         :down #(on-key-up!   key-states-atom %))))))
  (stop [this]
    (if running?
      (assoc this 
             :events (unbind-key-events events)
             :key-states nil
             :running? nil)
      this)))

(defn mk-keyboard []
  (map->Keyboard {}))
