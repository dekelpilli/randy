(ns randy.core
  (:refer-clojure :exclude [shuffle])
  (:require #?(:cljs [goog.array :as garray])
            [randy.rng :as rng])
  #?(:clj (:import (java.util Random))))

(def default-rng
  (delay
    #?(:cljs
       (reify rng/RandomNumberGenerator
         (next-int [_] (rand-int js/Number.MAX_VALUE))
         (next-int [_ upper] (rand-int upper))
         (next-int [_ lower upper] (+ lower (rand-int (- upper lower))))
         (next-double [_] (rand))
         (next-double [_ lower] (rand lower))
         (next-double [_ lower upper] (+ lower (rand (- upper lower)))))
       :clj (Random.))))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (mapv #(/ % total) ws)))

(def ^:private pop-to-nil (comp not-empty pop))

(defn alias-method-sampler
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings.
   The function returns indexes when values is null."
  ([weightings-map] (alias-method-sampler (keys weightings-map) (vals weightings-map)))
  ([values weightings]
   (assert (or (nil? values)
               (= (count weightings) (count values))))
   (let [values (when values (vec values))
         probabilities (weightings->probabilities weightings)
         n (count probabilities)
         avg (/ 1 n)
         base (vec (repeat n nil))
         indexes (reduce
                   (fn [acc i]
                     (let [p (nth probabilities i)]
                       (update acc (if (>= p avg) :large :small) (fnil conj []) i)))
                   {}
                   (range n))]
     (loop [probabilities probabilities
            alias base
            probability base
            {:keys [large small] :as indexes} indexes]
       (cond
         (and large small) (let [less (peek small)
                                 more (peek large)
                                 p-of-less (* n (nth probabilities less))
                                 p-of-more (- (+ (nth probabilities more) (nth probabilities less))
                                              avg)]
                             (recur (assoc probabilities more p-of-more)
                                    (assoc alias less more)
                                    (assoc probability less p-of-less)
                                    (-> indexes
                                        (update :large pop-to-nil)
                                        (update :small pop-to-nil)
                                        (update (if (>= p-of-more avg) :large :small) conj more))))
         small (recur probabilities alias
                      (assoc probability (peek small) 1)
                      (update indexes :small pop-to-nil))
         large (recur probabilities alias
                      (assoc probability (peek large) 1)
                      (update indexes :large pop-to-nil))
         :else (letfn [(generate-index
                         ([] (generate-index @default-rng))
                         ([rng]
                          (let [i (rng/next-int rng n)]
                            (if (<= (rng/next-double rng) (nth probability i))
                              i
                              (nth alias i)))))]
                 (cond->> generate-index
                          values (comp #(nth values %)))))))))

(defn weighted-sample
  ([m] (weighted-sample @default-rng m))
  ([rng m]
   (let [w (reductions #(+ % %2) (vals m))
         r (rng/next-double rng (last w))]
     (nth (keys m) (count (take-while #(<= % r) w))))))

(defn sample
  ([coll] (sample @default-rng coll))
  ([rng coll]
   (nth coll (rng/next-int rng (count coll)))))

(defn shuffle
  ([coll] (shuffle @default-rng coll))
  ([rng coll]
   #?(:clj  (let [al (java.util.ArrayList. ^java.util.Collection coll)
                  ^Random rnd (if (instance? Random rng)
                                rng
                                (proxy
                                  [Random] []
                                  (nextInt [_ upper] (rng/next-int rng upper))))]
              (java.util.Collections/shuffle al rnd)
              (clojure.lang.RT/vector (.toArray al)))
      :cljs (let [a (to-array coll)]
              (garray/shuffle a #(rng/next-double rng))
              (vec a)))))

(defn- shuffle-n-eager [rng n coll]
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
          (->> (dec remaining)
               (get in)
               (assoc! in idx)
               pop!
               (recur built remaining out)))))))

(def ^:dynamic *shuffle-strategy-pred*
  "Function deciding on strategy for sample-without-replacement."
  (fn [n coll] (> (/ n (count coll)) 2/11)))

(defn sample-without-replacement
  ([n coll] (sample-without-replacement @default-rng n coll))
  ([rng n coll]
   (let [coll (vec coll)]
     (if (*shuffle-strategy-pred* n coll)
       (subvec (shuffle rng coll) 0 n)
       (shuffle-n-eager rng n coll)))))
