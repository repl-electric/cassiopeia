(ns cassiopeia.engine.samples
  (:use overtone.live))

(def SAMPLE-ROOT "/Users/josephwilk/Workspace/music/samples/")

(defn load-local-sample [sample]
  (load-sample (str SAMPLE-ROOT sample)))
