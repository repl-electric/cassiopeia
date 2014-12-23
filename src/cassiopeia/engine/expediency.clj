(ns cassiopeia.engine.expediency
  (:require [shadertone.tone :as t])
  (:use [mud.core] [mud.chords] [overtone.live]))

(defn stop-everything! [] (remove-all-beat-triggers) (remove-all-sample-triggers) (stop-all-chord-synth-buffers) (full-stop))
(defn as-chord [note] (flatten [note 0 0 0]))
(defn chord-score [& score] (mapcat (fn [s] (if (sequential? (ffirst s)) (apply concat s) s)) score))
(defn chord-pattern! [& args] (apply chord-pattern args))

(alter-var-root (var ctl) (fn [f] (fn [& args] (if (and (map? (first args)) (:synths (first args)))
                                                 (apply f (:synths (first args)) (rest args))
                                                 (apply f args)))))

(def __graphics_state__ (atom {}))
(defn stop-graphics [file]
  (when (get @__graphics_state__ file)
    (t/stop)
    (swap! __graphics_state__ dissoc file)))

(defn start-graphics [& [file & args]]
  (if (get @__graphics_state__ file)
    (do
      (t/stop)
      (apply t/start-fullscreen file args))
    (do
      (swap! __graphics_state__ assoc file true)
      (apply t/start-fullscreen file args))))
