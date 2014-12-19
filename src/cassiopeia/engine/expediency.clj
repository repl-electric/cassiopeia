(ns cassiopeia.engine.expediency
  (:use [mud.core] [mud.chords] [overtone.live]))

(defn stop-everything! [] (remove-all-beat-triggers) (remove-all-sample-triggers) (stop-all-chord-synth-buffers) (full-stop))
(defn as-chord [note] (flatten [note 0 0 0]))
(defn chord-score [& score] (mapcat (fn [s] (if (sequential? (ffirst s)) (apply concat s) s)) score))
(defn chord-pattern! [& args] (apply chord-pattern args))

(alter-var-root (var ctl) (fn [f] (fn [& args] (if (and (map? (first args)) (:synths (first args)))
                                                 (apply f (:synths (first args)) (rest args))
                                                 (apply f args)))))
