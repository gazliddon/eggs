(ns eggs.fetch
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] )
  (:require 
    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]
    [cljs.core.async :as async ]))

(defn fetch 
  ([url]
   (let [ch (async/chan)]
     (doto (goog.net.XhrIo.)
       (.addEventListener goog.net.EventType.ERROR
                          (fn [event ]
                            (t/error (str "failed to load " url))
                            (async/close! ch)))

       (.addEventListener goog.net.EventType.COMPLETE 
                          (fn [event ]
                            (let [res (-> event .-target .getResponse)]
                              (t/info (str "loaded " url))
                              (go (async/>! ch res)
                                  (async/close! ch)
                                  ))))
       (.send url "GET")) 
     ch)))


(defn tagged-fetch [id url ]
  (go 
    {:id id 
     :url url
     :data ( async/<! (fetch url)) }))

(defn fetch-files-in-hash [hsh]
  (go 
    (loop [to-load (mapv #(apply tagged-fetch %) hsh)
           loaded {}]
      (if (empty? to-load)
        loaded
        (let [[{:keys [id data] :as v} c] (async/alts! to-load) ]
          (recur (filterv #(not= c %) to-load)
                 (assoc loaded id data )))))))

