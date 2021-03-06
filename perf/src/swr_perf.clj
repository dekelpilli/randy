(ns swr-perf
  (:require [clojure.test :refer [deftest is testing]]
            [randy.core :as r]
            [randy.rng :as rng]
            [incanter.core :as i]
            [incanter.charts :as ic])
  (:import (java.time Instant)
           (java.io File)))

;modified/modernised from https://gist.github.com/pepijndevos/805747
(defn take-shuffle [_ n coll]
  (subvec (shuffle coll) 0 n))

(defn take-filtered [rng n coll]
  (let [coll (vec coll)]
    (into [] (comp (distinct) (take n)) (repeatedly #(r/sample rng coll)))))

; reduce, reorder, subvec, O(m)
(defn take-reduce [rng n coll]
  (let [len (count coll)]
    (subvec (->> (range n)
                 (into [] (map (fn [n] [n (rng/next-int rng n len)])))
                 (reduce
                   (fn swap [v pair]
                     (let [i (first pair)
                           b (second pair)]
                       (assoc v b (get v i) i (get v b))))
                   coll))
            0 n)))

; amalloy, O(m)
(defn take-loop [rng n coll] ;mirrors old take-iterate
  (loop [candidates coll
         ret (transient [])
         c 1]
    (let [idx (rng/next-int rng (count candidates))
          ret (conj! ret (nth candidates idx))]
      (if (= c n)
        (persistent! ret)
        (recur (subvec (assoc candidates idx (first candidates)) 1)
               ret
               (inc c))))))

; amalloy, o(mg)
(defn take-transient [rng nr coll]
  (take nr
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (rng/next-int rng c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
         (transient coll))))

(defn shuffle-n-eager [rng n coll]
  (loop [out (transient [])
         in (transient coll)]
    (if (= n (count out))
      (persistent! out)
      (let [c (count in)
            idx (rng/next-int rng c)]
        (recur (conj! out (get in idx))
               (-> in
                   (assoc! idx (get in (dec c)))
                   (pop!)))))))

(defn shuffle-n-eager2 [rng n coll]
  (loop [built 0
         remaining (count coll)
         out (transient [])
         in (transient coll)]
    (let [built (inc built)
          idx (rng/next-int rng remaining)
          out (conj! out (get in idx))]
      (if (= n built)
        (persistent! out)
        (let [remaining (dec remaining)]
          (recur built
                 remaining
                 out
                 (-> in
                     (assoc! idx (get in (dec remaining)))
                     (pop!))))))))

(defmacro time* [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn time-f [f]
  (let [start (. System (nanoTime))]
    (f)
    (- (. System (nanoTime)) start)))

(defn plot-by-coll-size [f n]
  (let [coll (vec (range n))]
    (time-f #(doall (f r/default-rng 1000 coll)))))

(defn plot-by-n [coll-size f n]
  (let [coll (vec (range coll-size))]
    (time-f #(doall (f r/default-rng n coll)))))

(def x (range 1000 100000 1000))

(defn points [f g]
  (System/gc)
  (map #(f g %) x))

(defn draw-line [plot plotfn randfn name]
  (ic/add-points plot x (points plotfn randfn) :series-label name))

(defn do-it []
  (let [now (str (Instant/now))
        subdir (str "perf/" now)]
    (.mkdir (File. subdir))

    (-> (ic/scatter-plot [] [] :legend true :x-label "coll size" :y-label "nanoseconds")
        (draw-line plot-by-coll-size take-shuffle "shuffle")
        (draw-line plot-by-coll-size take-filtered "filtered")
        (draw-line plot-by-coll-size take-reduce "reduce")
        (draw-line plot-by-coll-size take-loop "iterate")
        (draw-line plot-by-coll-size take-transient "transient")
        (draw-line plot-by-coll-size shuffle-n-eager "shuffle-n-eager")
        (draw-line plot-by-coll-size shuffle-n-eager2 "shuffle-n-eager2")
        (i/save (str subdir "/len.png")))

    (let [f-1m #(plot-by-n 1000000 %1 %2)
          f-100k #(plot-by-n 100000 %1 %2)]
      (-> (ic/scatter-plot [] [] :legend true :x-label "n" :y-label "nanoseconds" :title "100k coll")
          (draw-line f-100k take-shuffle "shuffle")
          (draw-line f-100k take-filtered "filtered")
          (draw-line f-100k take-reduce "reduce")
          (draw-line f-100k take-loop "loop")
          (draw-line f-100k take-transient "transient")
          (draw-line f-1m shuffle-n-eager "transient-eager")
          (draw-line f-1m shuffle-n-eager2 "transient-eager2")
          (i/save (str subdir "/take100.png")))

      (-> (ic/scatter-plot [] [] :legend true :x-label "n" :y-label "nanoseconds" :title "1m coll")
          (draw-line f-1m take-shuffle "shuffle")
          (draw-line f-1m take-filtered "filtered")
          (draw-line f-1m take-reduce "reduce")
          (draw-line f-1m take-loop "loop")
          (draw-line f-1m take-transient "transient")
          (draw-line f-1m shuffle-n-eager "shuffle-n-eager")
          (draw-line f-1m shuffle-n-eager2 "shuffle-n-eager2")
          (i/save (str subdir "/take1m.png"))))))

(defn finalists []
  (->> (for [n (range 150000 300000 10000)
             s (range 100000 1000000 10000)
             :when (<= n s)]
         (let [coll (vec (range s))]
           {:s         s
            :n         n
            :shuffle   (time* (take-shuffle r/default-rng n coll))
            :transient (time* (doall (take-transient r/default-rng n coll)))
            :eager     (time* (doall (shuffle-n-eager r/default-rng n coll)))}))
       (sort-by (fn [x] [(:s x) (:n x)]))))
