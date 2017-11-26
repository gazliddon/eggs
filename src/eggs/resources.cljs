(ns eggs.resources
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] )

  (:require 
    [cljs.pprint :refer [pprint]]
    [eggs.fetch :as f]
    [plumbing.core :refer (fnk sum)] 
    [plumbing.graph-async :refer [async-compile]]
    [cljs.core.async :as a ]
    [plumbing.graph :as graph :refer [compile]] 
    [schema.core :as s]


    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf ]]
    )
  )

(enable-console-print!)

