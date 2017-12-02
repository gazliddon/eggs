(ns eggs.macros
  (:require 
    
    ))

(defn vb-h [gl vb shader uni]
  '((use-program! gl shader)
    (p/make-active! vb gl shader)
    (set-unis! gl shader unis)))

(defn shader-h [gl shader unis]
  '((use-program! gl shader)
    (set-unis! gl shader unis)))

(defmacro with-vb [gl vb shader unis & body]
  `(do
     ~@(vb-h gl vb shader unis)
     (do 
       ~@body )))

(defmacro with-shader [gl shader unis & body]
  `(do
     ~@(shader-h gl shader unis)
     (do 
       ~@body )))
