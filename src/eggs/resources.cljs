(ns eggs.resources
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] )

  (:require 
    [cljs.pprint :refer [pprint]]
    [eggs.fetch :as f]
    [plumbing.core :refer (fnk sum)] 
    [plumbing.graph-async :refer [async-compile]]
    [cljs.core.async :as async ]
    [plumbing.graph :as graph :refer [compile]] 
    [schema.core :as s]
    )
  )

(enable-console-print!)

;; shader prg
;; vs-text
;; fs-text

(def fetch-file-graph
  (->
    {:url (fnk[file] (str "shaders/" file) )
     :file-text (fnk [url] (f/fetch url)) } 
    async-compile))

(defn fetch-file [file]
  (fetch-file-graph {:file file}))

(def fetch-files-graph
  (->
    {:load-chans (fnk [files] (map fetch-file files))
     :loaded-files (fnk [load-chans]
                        (async/into [] (async/merge load-chans) )) }  
    async-compile ))

(defn fetch-files [files]
  (fetch-files-graph {:files ["line.vs" "line.fs"]}))

(comment 
  (go
    (let [ch (fetch-files [ [ "line.vs" "line.fs" ] ]) ]
      (do 
        (println "result")
        (println (async/<! ch))
        )
      )
    ) 
  )




