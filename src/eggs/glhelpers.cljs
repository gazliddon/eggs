(ns eggs.glhelpers
  (:require
    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf ]]
    [thi.ng.geom.gl.core :as gl]))

(defn gl-clear! 
  ([gl r g b a d]
   (gl/clear-color-and-depth-buffer gl r g b a d))

  ([gl r g b a]
   (gl-clear! gl r g b a 1))

  ([gl r g b]
    (gl-clear! gl r g b 1 1)))

(defn set-uni! [gl {:keys [uniforms]} k v]
  (if-let [uni (get uniforms k) ]
    (do 
      ((:setter uni) v))
    (t/warn (str "unknown uniform " k " val " v))))

(defn set-unis! [gl {:keys [uniforms] :as shader} hsh] 
  (doseq [[k v] hsh]
    (set-uni! gl shader k v)))

(defn use-program! [gl {:keys [program] :as shader } ]
  (.useProgram gl program))

