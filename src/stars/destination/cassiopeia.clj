(ns stars.cassiopeia
  (:use [overtone.live]
        [stars.warm-up]
        [stars.samples]
        [stars.engine.mixer]
        [overtone.synth.sampled-piano])
  (:require [stars.engine.timing :as tim]
            [stars.synths.synths :as syn]))

(def satellite-data
  "Noise from a satellite pointed towards the Cassiopeia star"
  [9 11 5 11 5 11 14 11 14 11 14 11 14 17 14 17 14 9 4 14 9 17
   14 17 9 21 9 4 14 4 9 4 9 21 4 9 21 4 9 14 17 14 17 14 17 9 17 14 17 0 17 0 17 0 14
   17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 4 0 4 0 4 0 4 0 4 0 17 9 17 14 17 4 14 9 4 14
   17 14 9 14 17 14 21 4 17 0 17 14 9 14 17 0 4 0 4 0 4 0 4 0 4 0 4 17 14 17 0 17 0 14 9
   14 4 17 0 4 9 4 9 14 9 14 9 4 9 4 9 4 14 0 17 0 14 17 14 17 0 14 0 14 17 14 17 14 17 14
   17 14 17 14 17 0 17 4 17 14 17 0 17 14 4 0 14 17 14 17 14 17 14 17 0 17 0 4 0 4 0 4 9
   4 9 4 9 14 19 14 19 0 5 0 5 9 14 19 2 11 19 2 7 16 21 7 11 16 4 16 21 4 9 21 9 0 9 14 2
   5 11 14 2 11 19 7 21 7 4 12 21 4 17 0 4 14 5 2 19 2 11 7 11 21 7 12 17 9 14 9 11 5 2 19
   11 7 21 7 16 12 9 17 0 19 9 14 2 11 19 7 16 11 7 16 4 16 12 21 9 16 21 4 17 4 0 19 0 17
   9 4 0 9 14 19 14 19 9 14 9 0 17 14 9 21 17 12 9 21 16 12 7 21 16 11 19 11 2 11 14 2 5 19
   9 4 14 9 16 21 2 7 21 7 11 16 9 14 12 9 4 21 4 21 19 2 16 19 5 14 19 9 14 0 9 21 4
   12 7 21 2 21 7 2 11 2 14 5 19 9 0 17 9 4 21 17 9 21 12 7 4 21 11 7 2 7 21 16 11 7 2 19 14
   5 2 19 14 9 5 19 14 9 4 0 17 9 17 0 14 17 14 9 14 9 4 9 14 17 9 4 9 14 9 14 9 14 9 14 17
   14 9 4 14 9 14 9 14 17 14 9 17 14 4 9 4 9 14 17 0 17 0 9 14 17 0 17 0 17 9 14 17 14 17
   9 14 9 14 17 14 17 14 17 14 17 14 17 14 17 14 9 14 9 14 9 14 17 14 17 14 17 14 17
   14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 17 0 17
   0 17 0 17 0 17 0 17 0 17 0 17 19 14 19 11 14 11 14 11 14])

(defsynth space-organ [out-bus 0]
  (let [f     (map #(midicps (duty:kr % 0 (dseq satellite-data INF)))
                   [1 1/2 1/4])
        tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/2) 3 4))
                   f
                   [1 4 8])]
    (out out-bus (g-verb (sum tones) 200 8))))

(defonce space-organ-g (group))
(def ob (nkmx :s1))

(space-organ [:head space-organ-g] :out-bus ob)

(stop)

(volume 0.90)

(do
  (defonce soundscape-sample (sample (freesound-path 38969)))
  (defonce soundscape (soundscape-sample :loop? true :rate 0.15)))

(ctl soundscape :rate 0.15 :out-bud (nkmx :s1))

(stop)
