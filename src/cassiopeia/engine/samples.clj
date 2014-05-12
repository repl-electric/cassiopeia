(ns cassiopeia.engine.samples
  (:use overtone.live))

(def SAMPLE-ROOT "/Users/josephwilk/Dropbox/repl-electric/samples/")

(defn load-local-sample     [sample] (load-sample (str SAMPLE-ROOT sample)))
(defn local-recording-start [name]   (recording-start (str SAMPLE-ROOT name)))
