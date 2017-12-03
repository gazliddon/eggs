(ns eggs.fontvb
  (:require 
    [cljs.pprint :refer [pprint]]
    [eggs.fontdata :as fdata]
    [eggs.lines :refer [add-lines mk-line-verts]]
    [thi.ng.geom.vector :as v :refer [vec2 vec3]]))

(enable-console-print!)

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

(defn mk-font [font-data hsh]
  (let [verts (-> (mk-line-verts hsh)
                  (assoc :frames {})) ]
    (reduce-kv 
      (fn [verts k letter]
        (let [start (count (:verts verts))
              verts (add-font-character verts letter) 
              num-of-verts (- (count (:verts verts) ) start) ]

          (update-in verts [:frames k] assoc 
                     :start start 
                     :num-of-verts num-of-verts)))
      verts font-data)))
