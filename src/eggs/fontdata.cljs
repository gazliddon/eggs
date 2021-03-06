(ns eggs.fontdata 
  (:require 
    [util.misc :refer [map-keys] ]
    )
  )

(def i->token {99 :marker 999 :marker} )

(defn mk-letter 
  "takes a whacky format letter and turns it into line strips"
  [l-vec]
  (->>
    l-vec
    (map #(get i->token % %))
    (partition-by keyword?)
    (remove (fn [[k]] (keyword? k)) )
    ;; end up with a vector of vectors of line strips
    (map #(partition 2 %))))

(def test-vec-font 
  {:A           [ -2 3  -2 -1  0 -3  2 -1  2 3  99  -2 0  2 0  999 ]
   :B           [ -2 3 -2 -3 1 -3 2 -2 2 -1 1 0 2 1 2 2 1 3 -2 3 99 -2 0 1 0 999 ]
   :C           [ 2 2 1 3 -1 3 -2 2 -2 -2 -1 -3 1 -3 2 -2 999 ]
   :D           [ -2 3 -2 -3 1 -3 2 -2 2 2 1 3 -2 3 999 ] })

(def vec-font {:A           [ -2 3  -2 -1  0 -3  2 -1  2 3  99  -2 0  2 0  999 ]

               :B           [ -2 3 -2 -3 1 -3 2 -2 2 -1 1 0 2 1 2 2 1 3 -2 3 99 -2 0 1 0 999 ]

               :C           [ 2 2 1 3 -1 3 -2 2 -2 -2 -1 -3 1 -3 2 -2 999 ]

               :D           [ -2 3 -2 -3 1 -3 2 -2 2 2 1 3 -2 3 999 ]

               :E           [ 2 3 -2 3 -2 -3 2 -3 99 -2 0 1 0 999 ]

               :F           [ -2 3 -2 -3 2 -3 99 -2 0 1 0 999 ]

               :G           [ 0 0 2 0 2 2 1 3 -1 3 -2 2 -2 -2 -1 -3 1 -3 2 -2 999 ]

               :H           [ -2 3 -2 -3 99 2 3 2 -3 99 -2 0 2 0 999 ]

               :I           [ 0 -3 0 3 99 -2 -3 2 -3 99 -2 3 2 3 999 ]

               :J           [ -2 2 -1 3 0 3 1 2 1 -3 0 -3 999 ]

               :K           [ -1 -3 -1 3 99 2 -3 -1 0 2 3 999 ]

               :L           [ -1.5 -3 -1.5 3 1.5 3 999 ]

               :M           [ -2 3 -2 -3 0 -1 2 -3 2 3 999 ]

               :N           [ -2 3 -2 -3 2 3 2 -3 999 ]

               :O           [ -1 3 -2 2 -2 -2 -1 -3 1 -3 2 -2 2 2 1 3 -1 3 999 ]

               :P           [ -2 3 -2 -3 1 -3 2 -2 2 -1 1 0 -2 0 999 ]

               :Q           [ -1 3 -2 2 -2 -2 -1 -3 1 -3 2 -2 2 2 1 3 -1 3 99 1 2 2 3 999 ] 

               :R           [ -2 3 -2 -3 1 -3 2 -2 2 -1 1 0 -2 0 99 0 0 2 3 999 ]

               :S           [ -2 2 -1 3 1 3 2 2 2 1 1 0 -1 0 -2 -1 -2 -2 -1 -3 1 -3 2 -2 999 ]

               :T           [ 0 -3 0 3 99 -2 -3 2 -3 999 ]

               :U           [ -2 -3 -2 2 -1 3 1 3 2 2 2 -3 999 ]

               :V           [ -2 -3 0 3 2 -3 999 ]

               :W           [ -2 -3 -2 3 0 1 2 3 2 -3 999 ]

               :X           [ -2 -3 2 3 99 -2 3 2 -3 999 ]

               :Y           [ -2 -3 0 -1 0 3 99 0 -1 2 -3 999 ]

               :Z           [ -2 -3 2 -3 -2 3 2 3 999 ]

               :0           [ -2 -3 2 -3 2 3 -2 3 -2 -3 999 ]

               :1           [ 0 -3 0 3 999 ]

               :2           [ -2 -3 2 -3 2 0 -2  0 -2  3 2 3 999 ]

               :3           [ -2 -3 2 -3 2 3 -2 3 99 -2 0 2 0 999 ]

               :4           [ -2 -3 -2 0 2 0 2 3 99 2 -3 2 0 999 ]

               :5           [ 2 -3 -2 -3 -2 0 2 0 2 3 -2 3 999 ]

               :6           [ 2 -3 -2 -3 -2 3 2 3 2 0 -2 0 999 ]

               :7           [ -2 -3 2 -3 2 3 999 ]

               :8           [ -2 0 -2 -3 2 -3 2 0 -2 0 -2 3 2 3 2 0 999 ]

               :9           [ 2 0 -2 0 -2 -3 2 -3 2 3 -2 3 999 ] 

               :caret       [-1 -3 -2 -0 0 1 2 0 1 -4 3 0 0 3 -3 0 -1 -3 999 ]

               :minus       [-1.5 0.0 1.5 0.0 999 ]

               :plus        [-1.5 0.0 1.5 0.0 99 0.0 2.0 0.0 -2.0 999 ]

               :underscore  [-2.0 3.0 2.0 3.0 999 ]

               :exclamation [-0.25 3.0 0.25 3.0 99 0.25 3.0 0.25 2.5000
                             99 0.25 2.5 -0.25 2.5 99 -0.25 2.5 -0.25
                             3.0 99 0.0 1.5 0.0 -2.5 999 ]

               :quotes      [ -1.5 -2.5 -1.5 0.0 99 1.5 0.0 1.5 -2.5 999 ]

               :diesis      [ -1.5 -1.0 1.5 -1.0 99 -1.5 1.0 1.5 1.0 99 -1.0 2.0 -0.5 -2.0 99 1.0 -2.0 0.5 2.0 999 ]

               :slash       [ 1.75 -2.75 -1.75 2.75 999 ]

               :percen      [ 1.75 -2.75 -1.75 2.75 99 1.25 0.5 0.75 1.0 
                             1.25 1.5 1.75 1.0 1.25 0.5 99  -1.25 -1.5 -0.75 -1.0 -1.25 -0.5 -1.75 -1.0 -1.25 -1.5 999 ]

               :parl        [ 0.5 -3.0 0.0 -2.5 99 0.0 -2.5 0.0 2.5 0.5 3.0 999 ]

               :parr        [ -0.5 -3.0 0.0 -2.5 99 0.0 -2.5 0.0 2.5 -0.5 3.0 999 ]

               :parql       [ 1.5 -2.5 0.0 -2.5 99 0.0 -2.5 0.0 2.5 99 0.0 2.5 1.5 2.5 999 ]

               :parqr       [ -1.5 -2.5 0.0 -2.5 99 0.0 -2.5 0.0 2.5 99 0.0 2.5 -1.5 2.5 999 ]

               :dollar      [ 1.25 -1.5 0.75 -2.0 99 0.75 -2.0 -0.75 -2.0 -1.25 -1.5 99 -1.25 -1.5 -1.25 
                             -0.5 -0.75 0.0 99 -0.75 0.0 0.75 0.0 1.25 0.5 99 1.25 0.5 1.25 1.5 0.75 2.0 99 
                             0.75 2.0 -0.75 2.0 -1.25 1.5 99 -0.25 -2.5 -0.25 2.5 99 0.25 2.5 0.25 -2.5 999 ]

               :asterisk    [ -1.5 0.0 1.5 0.0 99 -0.75 1.25 0.75 -1.25 99 -0.75 -1.25 0.75 1.25 999 ]

               :dot         [ -0.25 2.5 0.25 2.5 99 0.25 2.5 0.25 3.0 99 0.25 3.0 -0.25 3.0 99 -0.25 3.0 -0.25 2.5 999 ]

               :comma       [ -1.25 6.0 0.0 3.0 0.25 2.75 99 0.25 2.75 0.25 2.5 999 ]


               :semicolon   [ -0.25 3.0 0.0 3.0 0.25 2.75 99 0.25 2.75 0.25 2.5 99 
                             0.25 -0.75 0.25 -0.25 99  0.25 -0.25 -0.25 -0.25 99 -0.25 -0.25 -0.25 -0.75 99 -0.25 -0.75 0.25 -0.75 999 ]

               :colon       [ -0.25 2.5 0.25 2.5 99 0.25 2.5 0.25 2.0 99 0.25 2.0
                             -0.25 2.0 99 -0.25 2.0 -0.25 2.5 99  -0.25 -0.75 
                             0.25 -0.75 99 0.25 -0.75 0.25 -0.25 99 0.25 -0.25 -0.25 -0.25 99 -0.25 -0.25 -0.25 -0.75 999 ]

               :apostrophe  [ 0.0 -3.0 0.0 -1.0 999 ]

               :equal       [ -1.5 1.0 1.5 1.0 99 1.5 -1.0 -1.5 -1.0 999 ]

               :lessthan    [ 0.75 1.75 -1.25 0.0 0.75 -1.75 999 ]

               :greater     [ -0.75 1.75 1.25 0.0 -0.75 -1.75 999 ]

               :ampersand   [ 2.0 2.0 1.0 3.0 99 1.0 3.0 -1.0 3.0 -2.0 2.0 99 
                             -2.0 -2.0 -1.0 -3.0 99 -2.0 -2.0 -2.0 -1.0 99 
                             -1.0 -3.0 0.0 -3.0 1.0 -2.0 99 1.0 -2.0 1.0 -1.0 99 
                             -2.0 -1.0 2.0 3.0 99 1.0 -1.0 -2.0 1.0 99 -2.0 1.0 -2.0 2.0 999 ]

               :question    [ -1.5 -1.0 -1.5 -2.0 -0.75 -2.75 99 -0.75 -2.75 0.75
                             -2.75 1.5 -2.0 99 1.5 -2.0 1.5 -1.0 0.0 0.25 99 0.0 
                             0.25 0.0 2.0 99 -0.25 2.5 0.25 2.5 99 0.25 2.5 0.25 3.0 99 0.25 3.0 -0.25 3.0 99 -0.25 3.0 -0.25 2.5 999 ]

               :at          [ 2.0 1.0 2.0 -2.0 1.0 -3.0 99 
                             1.0 -3.0 -1.0 -3.0 -2.0 -2.0 99 -2.0 -2.0 -2.0 2.0 -1.0 3.0 99 
                             -1.0 3.0 2.0 3.0 99 2.0 1.0 -0.5 1.0 -1.0 0.5 99 -1.0 0.5 -1.0 
                             -1.0 -0.5 -1.5 99 -0.5 -1.5 0.5 -1.5 1.0 -1.0 99 1.0 -1.0 1.0 1.0 999 ]

               :pargright   [ -0.5 -3.0 0.0 -2.5 99 0.0 -2.5 0.0 -0.5 0.5 0.0 0.0 0.5 99 0.0 0.5 0.0 2.5 -0.5 3.0 999 ]

               :pargleft    [ 0.5 3.0 0.0 2.5 99 0.0 2.5 0.0 0.5 -0.5 0.0 0.0 -0.5 99 0.0 -0.5 0.0 -2.5 0.5 -3.0 999 ]

               :accent      [ -1.0 -3.0 0.0 -1.0 999 ]

               :backslash   [ 1.75 2.75 -1.75 -2.75 999 ]

               :bar         [ 0.0 -2.5 0.0 2.5 999 ]

               :tilde       [ -2.0 -1.5 -1.0 -2.5 1.0 -1.5 2.0 -2.5 999 ]

               :base        [ 1.0 3.0 -1.0 3.0 -2.0 2.0 99 -2.0 2.0 -2.0 0.0 -1.0 -1.0 99 
                             -1.0 -1.0 1.0 -1.0 2.0 0.0 99 1.0 3.0 2.0 2.0 999 ]
               })

(def vec-font-line-strips 
  (map-keys mk-letter vec-font))

(def test-vec-font-line-strips
  (map-keys mk-letter test-vec-font))


