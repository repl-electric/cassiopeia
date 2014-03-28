(ns cassiopeia.engine.core
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
