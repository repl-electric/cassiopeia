(ns stars.cassiopeia
  "                     /                /
 ___  ___  ___  ___    ___  ___  ___    ___
|    |   )|___ |___ | |   )|   )|___)| |   )
|__  |__/| __/  __/ | |__/ |__/ |__  | |__/|
                           |
Cassiopeia is a constellation in the northern sky, named after the vain queen Cassiopeia in Greek mythology, who boasted about her unrivalled beauty"
  (:use [overtone.live]
        [stars.warm-up]
        [stars.samples]
        [overtone.synth.sampled-piano])
  (:require [stars.engine.timing :as timing]
            [launchpad.sequencer :as lp-sequencer]
            [launchpad.plugin.beat :as lp-beat]
            [stars.engine.mixers :as m]
            [overtone.inst.synth :as s]
            [overtone.synths :as syn]))

(do
  (def star-into-the-sun (load-sample "~/Workspace/music/samples/star-into-the-sun.wav"))
  (def space-and-time-sun (load-sample "~/Workspace/music/samples/space_and_time.wav"))

  (def windy (sample (freesound-path 17553)))

  (def space-notes [8 16 32 16 8])
  (def space-tones [8 16 24])

  (defsynth high-space-organ [out-bus 0 amp 1 size 200 r 8 noise 10 trig 0 t0 8 t1 16 t2 24 d0 1 d1 1/2 d2 1/4 d3 1/8]
    (let [notes (map #(midicps (duty:kr % (mod trig 16) (dseq space-notes INF))) [d0 d1 d2 d3])
          tones (map (fn [note tone] (blip (* note tone)
                                          (mul-add:kr (lf-noise1:kr noise) 3 4))) notes [t0 t1 t2])]
      (out out-bus (* amp (g-verb (sum tones) size r)))))

  (defsynth plain-space-organ [out-bus 0 tone 1 duration 3 amp 1]
    (let [tones (map #(blip (* % 2) (mul-add:kr 1/8 1 4)) [tone])]
      (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE)))))

  (defsynth space-organ [out-bus 0 tone 1 duration 3 amp 1]
    (let [f     (map #(midicps (duty:kr % 0 (dseq 2 4)))
                     [1])
          tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/8) 2 4))
                     f
                     [tone])]
      (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE))))))

  ;;SCORE

(def sun (sample-player star-into-the-sun :rate 0.99 :amp 10 :out-bus (m/nkmx :s0)))

(def space-and-time (sample-player space-and-time-sun :rate 0.8))
(ctl space-and-time :rate 0.7)
(ctl space-and-time :rate 0.8)

(syn/fallout-wind)
(syn/soft-phasing :amp 0.1)

(kill syn/soft-phasing)
;;(space-organ :tone 24)
(plain-space-organ :tone 24)
(plain-space-organ :tone 22)
(plain-space-organ :tone 22)
(plain-space-organ :tone 24)

(def so (high-space-organ :amp 0.5 :trig timing/beat-cnt-bus :noise 220 :t0 2 :t1 4 :t2 8 :out-bus (m/nkmx :s0)))

(ctl so :noise 50)
(ctl so :vol 1)

(ctl so :t0 2 :t1 4 :t2 8)
(ctl so :t0 8 :t1 12 :t2 16)
(ctl so :t0 8 :t1 16 :t2 24)

(ctl so :d0 1 :d1 1/2 :d2 1/4 :d3 1/8)

(ctl so :r 10)
(ctl so :size 0)
(ctl so :size 200)
(ctl so :amp 0.1)

(stop)
