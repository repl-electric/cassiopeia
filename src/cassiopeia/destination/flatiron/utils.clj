(ns cassiopeia.destination.flatiron.utils
  (:require [mud.core :as mud])
  (:use overtone.live))

(defonce vol (atom 3.0))

(defn v [f] (reset! vol (float f)) (volume f))

(defn- traced-overtime!
  ([trace node field start end] (traced-overtime! trace node field start end 0.01))
  ([trace node field start end rate]
     (cond
      (and (map? node) (:synths node)) (traced-overtime! (:synths node) field start end rate)

      (sequential? node) (doseq [n node] (traced-overtime! n field start end rate))

      :else
      (letfn [(change-fn [val]  (if (< end start)
                                  (if (< (- val rate) end)
                                    end
                                    (- val rate))
                                  (if (> (+ val rate) end)
                                    end
                                    (+ val rate))))]
        (future
          (loop [val start]
            (Thread/sleep mud/overtime-default-sleep)
            (ctl node field val)
            (reset! trace (change-fn val))
            (when (not= val end)
              (recur (change-fn val)))))))))

(defn fadeout-master
  ([] (fadeout-master 1))
  ([current]
     (traced-overtime! vol (foundation-output-group) :master-volume current 0 0.05)))

(defn binary->pat [b](map #(Integer/parseInt %1) (clojure.string/split (Integer/toBinaryString b) #"")))

(defn spread
  "Euclidean distribution for beats

  ```
  (spread 1 4) => [1.0 0.0 0.0 0.0]
  ```
  "
  ([num-accents size] (spread num-accents size 0))
  ([num-accents size rotate-amount]
     (if (> num-accents size)
       (map (fn [_] 1.0) (range 0 size))
       (let [pattern (map #(< (mod (* %1 num-accents) size) num-accents) (range 0 size))]
         (loop [rotations rotate-amount
                res pattern]
           (if (> rotations 0)
             (let [res (rotate 1 res)
                   rotations  (if (= true (first res)) (dec rotations) rotations)]
               (recur rotations res))
             (map #(if %1 1.0 0.0) res)))))))
