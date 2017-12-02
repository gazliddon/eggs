(ns eggs.shaders 
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop ]] )
  (:require 
    [eggs.fetch :refer [fetch-files-in-hash]]
    [cljs.core.async :as async ]
    [thi.ng.geom.gl.shaders :as shaders]   ))

(defn async-load-shader [ gl shader ]
  (go
    (let [kz [:vs-file :fs-file :common]
          loaded (-> (select-keys shader kz)
                     (fetch-files-in-hash)
                     (async/<!)) 
          {:keys [common vs-file fs-file]} loaded 

          shader (assoc  shader
                        :vs (str common "\r\n" vs-file)
                        :fs (str common "\r\n" fs-file)
                        :vs-file nil
                        :fs-file nil
                        :common nil) ]
      (shaders/make-shader-from-spec gl shader ))))
