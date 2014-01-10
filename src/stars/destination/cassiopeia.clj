(ns stars.cassiopeia
  "                     /                /
 ___  ___  ___  ___    ___  ___  ___    ___
|    |   )|___ |___ | |   )|   )|___)| |   )
|__  |__/| __/  __/ | |__/ |__/ |__  | |__/|
                           |
Cassiopeia is a constellation in the northern sky, named after the vain queen Cassiopeia
in Greek mythology, who boasted about her unrivaled beauty

Bordered by:
* Andromeda to the south
* Perseus to the southeast
* Cepheus to the north.
"
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

  (defonce rhythm-g (group "Rhythm" :after timing/timing-g))
  (defonce saw-bf1 (buffer 256))
  (defonce saw-bf2 (buffer 256))

  (def space-notes-buf (buffer 5))
  (def space-dur-buf (buffer 4))
  (def space-tones-buf (buffer 3))

  (defonce saw-x-b1 (control-bus 1 "Timing Saw 1"))
  (defonce saw-x-b2 (control-bus 1 "Timing Saw 2"))
  (defonce saw-x-b3 (control-bus 1 "Timing Saw 3"))

  (defonce phasor-b1 (control-bus 1 "Timing Saw Phasor 1"))
  (defonce phasor-b2 (control-bus 1 "Timing Saw Phasor 2"))

  (defonce phasor-b3 (control-bus 1 "Timing Saw Phasor 3"))
  (defonce phasor-b4 (control-bus 1 "Timing Saw Phasor 4"))
  (defonce phasor-b5 (control-bus 1 "Timing Saw Phasor 5"))

  (defonce saw-s1 (timing/saw-x [:head rhythm-g] :out-bus saw-x-b1))
  (defonce saw-s2 (timing/saw-x [:head rhythm-g] :out-bus saw-x-b2))

  (defonce saw-s3 (timing/saw-x [:head rhythm-g] :out-bus saw-x-b3))

  (defonce phasor-s1 (timing/buf-phasor [:after saw-s1] saw-x-b1 :out-bus phasor-b1 :buf saw-bf1))
  (defonce phasor-s2 (timing/buf-phasor [:after saw-s2] saw-x-b2 :out-bus phasor-b2 :buf saw-bf2))

  (defonce phasor-s3 (timing/buf-phasor [:after saw-s3] saw-x-b3 :out-bus phasor-b3 :buf space-notes-buf))
  (defonce phasor-s4 (timing/buf-phasor [:after saw-s3] saw-x-b3 :out-bus phasor-b4 :buf space-dur-buf))
  (defonce phasor-s5 (timing/buf-phasor [:after saw-s3] saw-x-b3 :out-bus phasor-b5 :buf space-tones-buf))

  (defsynth buffered-plain-space-organ [out-bus 0 duration 4 amp 1]
    (let [tone (/ (in:kr phasor-b2) 2)
          tones (map #(blip (* % 2) (mul-add:kr (lf-noise1:kr 1/8) 1 4)) [tone])]
      (out out-bus (pan2 (* amp (g-verb (sum tones) 200 8))))))

  (defsynth ratatat [out-bus 0 amp 1]
    (let [freq (in:kr phasor-b2)
          sin1 (sin-osc (* 1.01 freq))
          sin2 (sin-osc (* 1 freq))
          sin3 (sin-osc (* 0.99 freq))
          src (mix [sin1 sin2 sin3])
          src (g-verb src :spread 10)]
      (out out-bus (* amp  (pan2  src)))))

  (defn transpose [updown notes]
    (map #(+ updown %1) notes))

  (def space-notes [8 16 32 16 8])
  (def space-tones [8 16 24])

  (defsynth crystal-space-organ [out-bus 0 amp 1 size 200 r 8 numharm 0 trig 0 t0 8 t1 16 t2 24 d0 1 d1 1/2 d2 1/4 d3 1/8]
    (let [notes (map  #(midicps (duty:kr % (mod trig 16) (dseq space-notes INF))) [d0 d1 d2 d3])
          tones (map (fn [note tone] (blip (* note tone) numharm)) notes [t0 t1 t2])]
      (out out-bus (* amp (g-verb (sum tones) size r)))))

  (comment  (def csp  (crystal-space-organ :numharm 0 :amp 0.5)))


  (defsynth high-space-organ [out-bus 0 amp 1 size 200 r 8 noise 10 trig 0 t0 8 t1 16 t2 24 d0 1 d1 1/2 d2 1/4 d3 1/8]
    (let [notes (map #(midicps (duty:kr % (mod trig 16) (dseq space-notes INF))) [d0 d1 d2 d3])
          tones (map (fn [note tone] (blip (* note tone)
                                          (mul-add:kr (lf-noise1:kr noise) 3 4))) notes [t0 t1 t2])]
      (out out-bus (* amp (g-verb (sum tones) size r)))))

  (high-space-organ)


  (defsynth timed-high-space-organ [out-bus 0 amp 1 size 200 r 8 noise 10 ]
    (let [space-notes [(in:kr phasor-b3) 16 32 16 8]
          duration    (in:kr phasor-b4)
          s-tone      (in:kr phasor-b5)
          all-tones   [s-tone (+ 8 s-tone) (+ 16 s-tone)]

          notes (map #(midicps (duty:kr % 0 (dseq space-notes INF))) [duration])
          tones (map (fn [note tone] (blip (* note tone)
                                          (mul-add:kr (lf-noise1:kr noise) 3 4))) notes all-tones)]
      (out out-bus (* amp (g-verb (sum tones) size r)))))

  (comment
    (ctl saw-s3 :freq-mul 1/256)
    (buffer-write! space-notes-buf [8 16 32 16 8])
    (buffer-write! space-tones-buf [8 8 8])
    (buffer-write! space-dur-buf   [1 1/2 1/4 1/8])

    (timed-high-space-organ :noise 100)
    (stop)
    )

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

(def sun (sample-player star-into-the-sun :rate 0.99 :amp 8 :out-bus (m/nkmx :s0)))

(def space-and-time (sample-player space-and-time-sun :rate 0.8))
(ctl space-and-time :rate 0.7)
(ctl space-and-time :rate 0.8)

(syn/fallout-wind)
(syn/soft-phasing :amp 0.1)
(def dark (syn/dark-sea-horns :amp 0.3))
(ctl dark :amp 0)

(kill dark)
(kill syn/soft-phasing)
(kill syn/fallout-wind)
;;(space-organ :tone 24)

;;Rythem

(def score   (map note [:F5 :G5 :G5 :G5 :G5 :BB5 :BB5 :D#5]))

;;(def score [[:F5 1/2] [:G5 2] [:BB5 1] [:D#5 1/2]])


(buffer-write! saw-bf2 (repeat 256 (midi->hz (note :A3))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ -10 midi-note))
                                 (map note (take 256 (cycle score))))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ -5 midi-note))
                                 (map note (take 256 (cycle score))))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ 0 midi-note))
                                 (map note (take 256 (cycle score))))))

(ratatat :amp 0.9)
(ctl saw-s2 :freq-mul 1/4000)
(kill ratatat)

(buffered-plain-space-organ :amp 0.8)
(kill buffered-plain-space-organ)

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

(def so (high-space-organ :amp 0.4 :trig timing/beat-count-b :noise 220 :t0 2 :t1 4 :t2 8 :out-bus (m/nkmx :s0)))

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
