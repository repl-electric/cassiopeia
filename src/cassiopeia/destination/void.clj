(ns cassiopeia.destination.void
  "A space for play"
  (:use [overtone.live]
        [cassiopeia.samples]
        [overtone.synth.sampled-piano]
        [cassiopeia.engine.buffers])
  (:require [cassiopeia.engine.timing :as timing]
            [launchpad.sequencer :as lp-sequencer]
            [cassiopeia.engine.mixers :as m]
            [overtone.inst.synth :as s]
            [overtone.synths :as syn]))

(def space-and-time-sun (load-sample "~/Workspace/music/samples/space_and_time.wav"))

(defsynth granulate [in-buf 0]
  (let [trate (mouse-y:kr 2 120 1)
        b in-buf
        dur (/ 1.2 trate)
        src (t-grains:ar :num-channels 1
                         :trigger (impulse:ar trate)
                         :bufnum b
                         :rate 1
                         :center-pos (mouse-x:kr 0 (buf-dur b))
                         :dur dur
                         :pan (* 0.6 (white-noise:kr))
                         :amp 0.1)]
    (out 0 src)))

(granulate :in-buf (buffer-mix-to-mono space-and-time-sun))

(defsynth noisemator [in-bus 0 out-bus 0]
  (let [f (in:ar in-bus)]
    (out out-bus (lf-noise1:ar f))))

(def main-b (audio-bus))

(sample-player space-and-time-sun :out-bus main-b :loop? 1)

(defsynth playit [in-bus 0 out-bus 0 amp 1]
  (let [src (in:ar in-bus 1)
        n (lf-noise0)]
    (out [0 1] (* amp n src) )))

(def noisey  (playit :in-bus main-b))

(ctl noisey :amp 0.9)

(defsynth glitchift [amp 1 out-bus 0]
  (let [l (local-in:ar 2)
        k (+ 1 (* l (lf-saw:ar l 0)))
        j (range-lin k 0.25 4.0)
        s (pitch-shift:ar (sin-osc-fb (pow j [l k]) k) [0.05 0.03] j)]
    (local-out:ar s)
    (out out-bus (* amp (pan2 s)))))

(glitchift)
(stop)

(defsynth drone [amp 1 out-bus 0]
  (let [l (local-in:ar 2)
        k (+ 1 (* l (lf-saw:ar l 0)))
        j (range-lin:ar 0.25 4.0)
        s (pitch-shift (sin-osc-fb:ar (pow j [2 2.1]) k) [0.05 0.03] j)]
    (local-out:ar s)
    (out out-bus (* amp (pan2 s)))))

(def d (drone))

(stop)


(defsynth p [out-bus 0 freq 400 amp 0.1 gate 1 detune 0]
  (let [env (env-gen:ar (env-perc (ranged-rand 0.001 0.01) (lin-rand 0.2 0.4) amp (ranged-rand -9 -1) 2))
        snd (mix (* (* env (* 2 Math/PI)) (sin-osc:ar freq [0 (* 0.01 detune)])))]
    (out out-bus (pan2:ar (* snd env (ranged-rand -1 1))))))

(defsynth t [amp 1]
  (let [mod (+ (duty:ar 1 0 (* (dseq [0 8 1 5]) [1 4 8])) (* (+ 128 (* 32 (saw:ar 1))) (saw:ar [3 4])))
        snd (/ (+ mod (* 99 (sin-osc:ar 2 64))) 9)
        src (comb-n snd 1/4 (/ 1 4.125) (range-lin (sin-osc:kr 0.005 (* 1.5 Math/PI)) 0 6))]
    (out 0 (* amp src))))

(stop)
(p)

(t :amp 0.1)

(defsynth syn [out-bus 0 cnt 1]
  (let [del (* 1 (delay-n:ar (in-feedback:ar 0 2) (in-feedback:ar 100 2) 1))
        src (/ (sin-osc:ar (+ (* cnt 99) [0 2]) (range-lin del 1 0)) 4)]
    (out out-bus (pan2 src))))

(stop)
(def s (syn))
(ctl s :cnt 4)
(ctl s :cnt (- 9 2))
