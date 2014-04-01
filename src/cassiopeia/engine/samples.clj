(ns cassiopeia.engine.samples
  (:use overtone.live))

(def SAMPLE-ROOT "/Users/josephwilk/Dropbox/repl-electric/samples/")

(defn load-local-sample [sample]
  (load-sample (str SAMPLE-ROOT sample)))
