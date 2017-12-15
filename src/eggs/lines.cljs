(ns eggs.lines 
  (:require ))

(defprotocol ILines
  (set-base [this new-base])
  (add-line-v [this new-v])
  (add-line-p [this p0 p1]))

(defprotocol IVerts
  (get-verts [this]))

(defrecord Verts [base verts]
  ILines
   (set-base [this new-base]
    (assoc this :base new-base))

  (add-line-v [this new-v]
   (let [new-base (merge base new-v)
         new-verts (map #(assoc new-base :a_index %) [1 0 2 0 2 3]) ]
      (assoc this :verts (into verts new-verts) :base new-base )) )

  (add-line-p [this p0 p1]
    (add-line-v this {:a_position0 p0 :a_position1 p1})))

(defn mk-line-verts [ base-hash ]
  (map->Verts {:base base-hash :verts []}))

(defn add-lines [verts lines]
  (loop [verts verts lines lines ]
    (if (> (count lines) 1)
      (recur
        (add-line-p verts (first lines) (second lines))
        (rest lines))
      verts)))
