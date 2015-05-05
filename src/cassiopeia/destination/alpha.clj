(ns cassiopeia.alpha
"

      .'.       |        |`````````, |         |       .'.
    .''```.     |        |'''''''''  |_________|     .''```.
  .'       `.   |        |           |         |   .'       `.
.'           `. |_______ |           |         | .'           `.

* Surface temperature: 4530 K
* Mass: 8.95E30 kg
* Radius: 29,280,000 km
* Magnitude: 2.24
"
  (:use [overtone.live]
        ;;[cassiopeia.warm-up]
        [cassiopeia.samples]
        [overtone.synth.sampled-piano]
        [mud.core])
  (:require [mud.timing :as time]
            ;;[launchpad.sequencer :as lp-sequencer]
            ;;[launchpad.plugin.beat :as lp-beat]
            [cassiopeia.engine.expediency :refer :all]
            [cassiopeia.engine.mixers :as m]
            [overtone.inst.synth :as s]
            [overtone.synths :as syn]
            [cassiopeia.waves.synths :as cs]))

(do
  (def star-into-the-sun (load-sample "~/Workspace/music/samples/star-into-the-sun.wav"))
  (def space-and-time-sun (load-sample "~/Workspace/music/samples/space_and_time.wav"))

  (def windy (sample (freesound-path 17553)))

  (defonce rhythm-g (group "Rhythm" :after time/timing-g))
  (defonce saw-bf1 (buffer 256))
  (defonce saw-bf2 (buffer 256))

  (defonce saw-x-b1 (control-bus 1 "Timing Saw 1"))
  (defonce saw-x-b2 (control-bus 1 "Timing Saw 2"))
  (defonce saw-x-b3 (control-bus 1 "Timing Saw 3"))

  (defonce phasor-b1 (control-bus 1 "Timing Saw Phasor 1"))
  (defonce phasor-b2 (control-bus 1 "Timing Saw Phasor 2"))

  (defonce phasor-b3 (control-bus 1 "Timing Saw Phasor 3"))
  (defonce phasor-b4 (control-bus 1 "Timing Saw Phasor 4"))
  (defonce phasor-b5 (control-bus 1 "Timing Saw Phasor 5"))

  (defonce saw-s1 (time/saw-x [:head rhythm-g] :out-bus saw-x-b1))
  (defonce saw-s2 (time/saw-x [:head rhythm-g] :out-bus saw-x-b2))

  (defonce saw-s3 (time/saw-x [:head rhythm-g] :out-bus saw-x-b3))

  (defonce phasor-s1 (time/buf-phasor [:after saw-s1] saw-x-b1 :out-bus phasor-b1 :buf saw-bf1))
  (defonce phasor-s2 (time/buf-phasor [:after saw-s2] saw-x-b2 :out-bus phasor-b2 :buf saw-bf2))

  (def space-notes-buf (buffer 5))
  (def space-tones-buf (buffer 3))

  (defonce phasor-s3 (time/buf-phasor [:after saw-s3] saw-x-b3 :out-bus phasor-b3 :buf space-notes-buf))

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

  (def space-notes [8 16 32 16 8])
  (defsynth high-space-organ [cutoff 90 out-bus 0 amp 1 size 200 r 8 noise 10 trig 0 t0 8 t1 16 t2 24 d0 1 d1 1/2 d2 1/4]
    (let [space-notes1-buf (buf-rd:kr 1 space-notes-buf 0)
          space-notes2-buf (buf-rd:kr 1 space-notes-buf 1)
          space-notes3-buf (buf-rd:kr 1 space-notes-buf 2)
          space-notes4-buf (buf-rd:kr 1 space-notes-buf 3)
          space-notes5-buf (buf-rd:kr 1 space-notes-buf 4)
          space-notes-in [space-notes1-buf space-notes2-buf space-notes3-buf space-notes4-buf space-notes5-buf]
          notes (map #(duty:kr % (mod trig 16) (dseq space-notes-in INF)) [d0 d1 d2])
          tones (map (fn [note tone]
                       (println :none note :tone tone)
                       (blip (* note tone) (mul-add:kr (lf-noise1:kr noise) 3 4))) notes [t0 t1 t2])
          _ (println tones)
          src (* amp (g-verb (sum tones) size r))
          src (lpf src cutoff)]
      (out out-bus src)))

  (println (map midi->hz [8 16 32 16 8]))

(ctl so :t0 1 :t1 1 :t2 1)

(pattern! space-notes-buf (map midi->hz (degrees-seq [:C#0 1 :C#0 3 :C#1 5])))

  ;;:t0 2 :t1 4 :t2 8 :out-bus 0
  (pattern! space-notes-buf (map midi->hz [8 16 32 16 8]))
  (def so (high-space-organ :amp 0.4
                            :trig time/beat-count-b
                            :noise 220 :t0 1 :t1 1 :t2 2 :out-bus 0))
  (ctl so :cutoff 2000a)
  (ctl so :amp 0.5)
  (ctl so :t0 2)
  (ctl so :t1 4)
  (ctl so :t2 8)
  (kill high-space-organ)

  (comment (high-space-organ))

  (defsynth timed-high-space-organ [out-bus 0 amp 1 size 200 r 8 noise 10 ]
    (let [note (in:kr phasor-b3)
          tone1 (buf-rd:kr 1 space-tones-buf 0)
          tone2 (buf-rd:kr 1 space-tones-buf 1)
          tone3 (buf-rd:kr 1 space-tones-buf 2)

          trig1 (t-duty:kr (dseq [1] INFINITE))
          trig2 (t-duty:kr (dseq [1/2] INFINITE))
          trig3 (t-duty:kr (dseq [1/4] INFINITE))

          note1 (midicps (demand:kr trig1 0 (drand note INFINITE)))
          note2 (midicps (demand:kr trig2 0 (drand note INFINITE)))
          note3 (midicps (demand:kr trig3 0 (drand note INFINITE)))

          all-tones [tone1 tone2 tone3]
          all-notes [note1 note2 note3]

          tones (map (fn [note tone] (blip (* note tone)
                                          (mul-add:kr (lf-noise1:kr noise) 3 4))) all-notes all-tones)]
      (out out-bus (* amp (g-verb (sum tones) size r)))))

  (comment
    (show-graphviz-synth timed-high-space-organ)
    (ctl saw-s3 :freq-mul 1/32)
    (buffer-write! space-notes-buf [8 16 32 16 8])

    (buffer-write! space-tones-buf [2 4 8])
    (buffer-write! space-tones-buf [8 12 16])
    (buffer-write! space-tones-buf [8 16 24])

    (def thso (timed-high-space-organ :noise 220 :amp 0.4))

    (ctl thso :noise 10)
    (ctl thso :size 0)
    (ctl thso :size 200)

    (def so (high-space-organ :amp 0.4 :trig time/beat-count-b :noise 220 :t0 2 :t1 4 :t2 8 :out-bus 0))

    (show-graphviz-synth high-space-organ)

    (stop))

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

(def sun (sample-player star-into-the-sun :rate 0.99 :amp 8 :out-bus 0))

(def space-and-time (sample-player space-and-time-sun :rate 0.8))
(ctl space-and-time :rate 0.7)
(ctl space-and-time :rate 0.8)

(syn/fallout-wind)
(syn/soft-phasing :amp 0.0)
(def dark (syn/dark-sea-horns :amp 0.3))
(ctl dark :amp 1)

(kill dark)
(kill syn/soft-phasing)
(kill syn/fallout-wind)

;;Rythm

(def score   (map note [:F5 :G5 :G5 :G5 :G5 :BB5 :BB5 :D#5]))

(buffer-write! saw-bf2 (repeat 256 (midi->hz (note :A3))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ -12 midi-note))
                                 (map note (take 256 (cycle score))))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ -5 midi-note))
                                 (map note (take 256 (cycle score))))))

(buffer-write! saw-bf2 (map midi->hz
                            (map (fn [midi-note] (+ 0 midi-note))
                                 (map note (take 256 (cycle score))))))

(ratatat :amp 0.9)
(ctl saw-s2 :freq-mul 1/40)
(kill ratatat)

(buffered-plain-space-organ :amp 0.8)
(kill buffered-plain-space-organ)

(stop)

;;Jaming

(plain-space-organ :tone (/ 24 2) :duration 16)

(def note-cycle (degrees [1 2 3] :major :C#2))
(def note-inc (atom 0))
(def trigger-g99018
  (on-beat-trigger 8 (fn []
                       (swap! note-inc inc)
                       (plain-space-organ :tone (/ (midi->hz (nth (degrees [1 2 3] :major :C#1) (mod @note-inc 3) )  ) 1) :duration 16.0 :amp 0.2)
                        )))
(remove-beat-trigger trigger-g99018)
(remove-all-beat-triggers)


(def so (high-space-organ :amp 0.4 :trig time/beat-count-b :noise 220 :t0 2 :t1 4 :t2 8 :out-bus 0))
(def so (high-space-organ :amp 0.4 :trig time/beat-count-b :noise 220 :t0 1 :t1 1 :t2 1 :out-bus 0))

(ctl so :cutoff 250)

(kill so)

(ctl so :noise 50)
(ctl so :vol 1)

(ctl so :t0 1 :t1 1 :t2 1)
(ctl so :t0 8 :t1 12 :t2 16)
(ctl so :t0 8 :t1 16 :t2 24)

(ctl so :r 10)
(ctl so :size 0)
(ctl so :size 200)
(ctl so :amp 0.1)


(volume 1.0)
(comment (stop))
(comment
  (defonce kick-seq-buf (buffer 256))
  (def beats (cs/buffer->tap-lite kick-seq-buf (:count time/beat-1th) :measure 8))

  (start-graphics "resources/shaders/space_and_time.glsl"
                  :textures [:overtone-audio
                             :previous-frame
                             "resources/textures/tex16.png"
                             "resources/textures/time.png"
                             ;;"resources/textures/space.png"
                             ]
                  :user-data {"iGlobalBeatCount" (atom {:synth beats :tap "global-beat-count"})})
  (stop-graphics "resources/shaders/space_and_time.glsl")
)
