(ns eggs.printables
  (:require
    [thi.ng.geom.gl.webgl.constants :as glc]
    [thi.ng.geom.gl.shaders :as shaders] 
    [thi.ng.geom.gl.core :as gl]))

(defonce printables (atom {}))

(defn- update-hash [old-hash fun]
  (let [new-hash (reduce 
                   (fn [new-hash [k v]]
                     (assoc! new-hash k (get old-hash k)))
                   (transient {})
                   old-hash)]
    (persistent! new-hash)))


(defn get-printable [k]
  (get @printables k))

(defn- model->printable [ctx model shader-spec]
  (-> model
      (gl/as-gl-buffer-spec {})
      (assoc :shader (shaders/make-shader-from-spec ctx shader-spec))
      (gl/make-buffers-in-spec ctx glc/static-draw)) )

(defn register-printable! [ctx k model shader-spec]
  (let [p (model->printable ctx model shader-spec)]
    (swap! printables assoc k p)
    p))

(defn update-printable-uniforms [p & kz]
  (assoc p :uniforms (apply assoc (:uniforms p) kz))  )

(defn update-uniforms! [ctx & kz]
  (->>
    (fn [p ] (update-printable-uniforms p kz))
    (update-hash @printables)
    (reset! printables)))

(defn draw-with-uniforms [ctx k & kz]
  (let [p (get @printables k) ]
    (gl/draw-with-shader 
      ctx
      (update-printable-uniforms p kz))))

(defn draw [ctx k model-mat]
  (draw-with-uniforms ctx k :model model-mat))


