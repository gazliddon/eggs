(ns util.misc
  (:require 
    [cljs.pprint :refer [pprint]])
  )

(defn map-keys [func hsh ]
  (reduce-kv (fn [o k v] (assoc o k (func v)))  {} hsh))

(defn map-kv [func hsh]
  (reduce-kv (fn [acc k v]
               (assoc acc k (func k v))) {} hsh))

(defn ->pprint 
  ([v txt]
   (println txt)
   (pprint v) 
   v)
  ([v] (pprint v) v))

(defn js-log [v] (.log js/console v))

(defn compare-indexed
  "Compare indexed collection."
  ([xs ys]
     (let [xl (count xs)
           yl (count ys)]
       (cond
        (< xl yl) -1
        (> xl yl) 1
        (== xl 0) 0
        :else (compare-indexed xs ys xl 0))))
  ([xs ys len n]
     (let [d (compare (nth xs n) (nth ys n))]
       (if (and (zero? d) (< (+ n 1) len))
         (recur xs ys len (inc n))
         d))))

(defn equiv-indexed 
  "equivalent indexed"
  [a b]
  (cond 
    (not (satisfies? cljs.core/IIndexed a)) false
    (not (satisfies? cljs.core/IIndexed b)) false
    (not (satisfies? cljs.core/IComparable a)) false
    (not (satisfies? cljs.core/IComparable b)) false
    :else (zero? (compare-indexed a b))))


;; {{{ Helpers hex printer
(def n-hex-char "0123456789abcdef")
(defn get-hex-ch [n] (nth n-hex-char (bit-and 0xf n)))
(defn hex-str [i]
  (if (= 0 i)
    "0x0"
    (loop [ret "" i (int i)]
      (if (pos? i)
        (recur 
          (.concat (get-hex-ch i) ret)
          (bit-shift-right i 4))
        (.concat "0x" ret)))) )

(defn hex-array-str [data]
  (str  (mapv hex-str data)))
;; }}}
