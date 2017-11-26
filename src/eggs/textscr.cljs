(ns eggs.textscr
  (:require
    ))


; (defprotocol ITextScr
;   (get-wh [_])
;   (clear! [_])
;   (box! [_ xy wh])
;   (ch-set! [_ c xy])
;   (ch-get [_ xy]))

; (defn- in-rect? [[x y] [w h]]
;   (and
;     (>= x 0)
;     (< x w)
;     (>= y 0)
;     (< y h)))

; (defn- clamp-num [n n-min n-max]
;   (cond 
;     (< n n-min) n-min
;     (> n n-max) n-max
;     :else n))


; (defn- mk-char [ch p fx fy]
;  {:char ch  :palette p :flip-x fx :flip-y fy} )

; (defn- mk-space []
;   (mk-char \  0 false false))

; (defrecord TextScr [wh arr null-ch]
;   ITextScr

;   (clear! [_])

;   (get-wh [_] wh )

;   (box! [_ xy box-wh ch]
;     (let [[bx by] xy
;           [bw bh ] box-wh
;           [bx1 by1] [(+ bx (dec bw)) (+ by (dec bh))]]
;       )
;     )

;   (ch-set! [this c xy]
;     (when (in-rect? xy wh)
;       (let [[x y] xy
;             [w h] wh ]
;         (swap arr (assoc @arr (+ x (* y w) ) c)))
;       this))

;   (ch-get [this xy]
;     (when (in-rect? xy wh)
;      (let [[x y] xy
;            [w h] wh ]
;         (get @arr (+ x (* y w) ))) 
;      null-ch)))


; (defn mk-text-scr [[w h]]
;   (->
;     (->TextScr [w h] arr {:char \@ :palette 0 :flip-x false :flip-y false})
;     (clear!)))
