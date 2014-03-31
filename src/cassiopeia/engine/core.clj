(ns cassiopeia.engine.core
  "Layers over Overtone which make composition more immediate"
  (:use [overtone.live]))

(defn pattern!
  "Fill a buffer repeating pattern if required.
     Supports integers or notes which will be converted to midi notes"
  [buf & lists]
      (buffer-write! buf (take (buffer-size buf) (cycle (map #(if (keyword? %) (note %) %) (flatten lists))))))

(defn pattern-seq!
  "Fill a buffer repeating pattern if required. Support expressing patterns with `x` and `o`.
     For example: `oooxxoo`"
  [buf & lists]
  (let [buf-lists (map (fn [list] (if (string? list)
                                    (map #(Integer/parseInt %)
                                         (-> list
                                             (clojure.string/replace #"o" "0")
                                             (clojure.string/replace #"x" "1")
                                             (clojure.string/split #"")))
                                    list))
                       lists)]
          (pattern! buf buf-lists)))

(defn node-over-time
  "Over time change val of `field` to end"
  [node field start end rate]
  (loop [vol start]
    (when (>= vol end)
      (println vol)
      (Thread/sleep 200)
      (ctl node field vol)
      (recur (- vol rate)))))

(defn over-time!
  ([thing towards] (over-time thing towards 0.1))
  ([thing towards rate]
      (letfn [(change-fn [val]  (if (= towards 0)
                                  (if (< (- val rate) towards)
                                    towards
                                    (- val rate))
                                  (if (> (+ val rate) towards)
                                    towards
                                    (+ val rate))))]
        (future (loop []
                  (when (not= @thing towards)
                    (Thread/sleep 200)
                    (swap! thing change-fn)
                    (println @thing)
                    (recur)))))))
