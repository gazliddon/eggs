(ns util.vec
  (:require [util.protocols :as p])
  )

(defn get-storage-f [^Integer n]  
  #?(:cljs
     (js/Float32Array. n)
     :clj
     "TBD"))

(defn clone-storage-f [b]
  #?(:cljs
     (js/Float32Array. b)
     :clj
     "tbd"))

(defn storage-f-2-op [op a b n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v (op (aget a i) (aget b i))))
      (clone-storage-f v))))

(defn storage-f-2-op-scalar [op a scalar n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v (op (aget a n) scalar)))
      (clone-storage-f v))))

(defprotocol IVec3
  (x [_])
  (y [_])
  (z [_]))

(declare v32op)

(defrecord V3F [buf]
  p/IMathOps

  (+ [this b] (v32op + this b))
  (- [this b] (v32op - this b) )
  (* [this b] (v32op * this b) )
  (div [this b] (v32op / this b) )

  IVec3
  (x [this] (aget buf 0))
  (y [this] (aget buf 1))
  (z [this] (aget buf 2 )))

(defn vec3f

  ([^V3F v]
   (->V3F (clone-storage-f (:buf v))))

  ([^double x ^double y ^double z]
   (->V3F 
     (clone-storage-f [x y z])))

  ([]
   (->V3F (get-storage-f 3))))

(defn v32op [op a b]
  (->V3F 
    (storage-f-2-op op (:buf a) (:buf b) 3) ))

