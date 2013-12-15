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

;;Rythem
(defonce rhythm-g (group "Rhythm" :after timing/timing-g))
(defonce saw-bf1 (buffer 256))
(defonce saw-bf2 (buffer 256))

(defonce saw-x-b1 (control-bus 1 "TIM Saw"))
(defonce saw-x-b2 (control-bus 1 "TIM Saw2"))

(defonce phasor-b1 (control-bus 1 "TIM Saw Phsr"))
(defonce phasor-b2 (control-bus 1 "TIM Saw Phsr 2"))

(defonce saw-s1 (timing/saw-x [:head rhythm-g] :out-bus saw-x-b1))
(defonce saw-s2 (timing/saw-x [:head rhythm-g] :out-bus saw-x-b2))

(defonce phasor-s1 (timing/buf-phasor [:after saw-s1] saw-x-b1 :out-bus phasor-b1 :buf saw-bf1 :loop? true))
(defonce phasor-s2 (timing/buf-phasor [:after saw-s2] saw-x-b2 :out-bus phasor-b2 :buf saw-bf2 :loop? true))

(defsynth buffered-plain-space-organ [out-bus 0 duration 4 amp 1]
  (let [tone (in:kr phasor-b2)
        tones (map #(blip (* % 2) (mul-add:kr 1/8 1 4)) [tone])]
    (out out-bus (pan2 (* amp (g-verb (sum tones) 200 8))))))

(buffer-write! saw-bf2 (repeat 256 24))
(buffer-write! saw-bf1 (repeat 256 1))

(buffer-write! saw-bf2 (map (fn [note] (+ 0 note))
                            (map note (flatten
                                       (concat (repeat 64 [:A4])
                                               (repeat 64 [:B5])
                                               (repeat 64 [:C6])
                                               (repeat 64 [:C2]))))))

(ctl timing/divider-s :div 1)

(ctl timing/root-s :rate 2)
(buffered-plain-space-organ :amp 1)
(kill buffered-plain-space-organ)

(ctl saw-s2 :freq-mul 1 :add 1 :mul 1 :rate 1)


;;Jaming

(do
  (plain-space-organ :tone 24 :duration 3)
  (plain-space-organ :tone 28 :duration 8)
  (plain-space-organ :tone 22 :duration 1)
  (plain-space-organ :tone 24 :duration 1)
  (plain-space-organ :tone 20 :duration 1)
  (plain-space-organ :tone 22 :duration 1)
  (plain-space-organ :tone 22 :duration 1)
  (plain-space-organ :tone 20 :duration 3))

(def so (high-space-organ :amp 0.5 :trig timing/beat-cnt-bus :noise 220 :t0 2 :t1 4 :t2 8 :out-bus (m/nkmx :s0)))

(kill so)

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

(comment (stop))
