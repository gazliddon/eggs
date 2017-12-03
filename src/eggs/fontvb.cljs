(ns eggs.fontvb
  (:require 
    [eggs.fontdata :as fdata]
    [eggs.lines :refer [add-lines mk-line-verts]]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

(defn to-verts [l-chunk]
  (map #(apply vec3 %) l-chunk))

(defn mk-letter [l-vec]
  (->>
    l-vec
    (map #(if (or (= % 99) (= % 999)) :marker %))
    (partition-by keyword?)
    (remove (fn [[k]] (keyword? k)) )
    (map #(partition 2 %))))

(defn add-font-character [verts lines ] 
  (let [lines (->> (mk-letter lines)
                   (map to-verts)) ]
    (->
      (fn [verts l-chunk] (add-lines verts l-chunk))
      (reduce verts lines))))

(defn mk-font [font-data]
  (let [verts (mk-line-verts {}) ]
    (reduce-kv 
      (fn [[verts line-offsets] k letter]
        [(add-font-character verts letter)
         (assoc line-offsets k {:start 0 :num-of-verts 0}) ])
      [verts {}] font-data)))
