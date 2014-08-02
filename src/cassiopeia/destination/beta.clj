(ns cassiopeia.destination.beta
  "
.--.      .
|   )    _|_
|--:  .-. |  .-.
|   )(.-' | (   )
'--'  `--'`-'`-'`-

Music for the journey"
  (:use [overtone.live]
        [cassiopeia.warm-up]
        [cassiopeia.samples]
        [overtone.synth.sampled-piano])
  (:require [mud.timing :as timing]
            [launchpad.sequencer :as lp-s]
            [launchpad.plugin.beat :as lp-beat]
            [overtone.synths :as syn]
            [overtone.inst.synth :as s]
            [cassiopeia.engine.sequencer :as sequencer]))

(def beats-g (group "beats"))

(def dum-samples-set [beatbox-kick-s tom-s shake-s shake2-s shake1-s shake2d-s clap-s clap2-s])

(sequencer/swap-samples! sequencer-64 dum-samples-set)

(lp-s/sequencer-write! sequencer-64 0 [1 0 0 0 0 1 0 0])

(lp-s/sequencer-write! sequencer-64 2 [0 1 0 0 0 0 0 0])
(lp-s/sequencer-write! sequencer-64 3 [0 0 0 1 0 0 0 0])
(lp-s/sequencer-write! sequencer-64 4 [0 0 0 0 1 0 0 0])

(lp-s/sequencer-write! sequencer-64 5 (take 8 (cycle [1])))
(lp-s/sequencer-write! sequencer-64 5 [0 0 0 0 0 0 0 1])

(do
  (lp-s/sequencer-write! sequencer-64 1 [0 0 1 0 0 0 1 0])
  (lp-s/sequencer-write! sequencer-64 5 [0 0 0 0 0 0 0 1]))

(do
  (lp-s/sequencer-write! sequencer-64 6 [1 0 0 0 0 0 0 0])
  (lp-s/sequencer-write! sequencer-64 7 [0 0 0 0 0 0 0 1])
  (lp-s/sequencer-write! sequencer-64 5 [0 0 0 0 0 0 0 1]))

(defonce score-b         (buffer 128))
(defonce duration-b      (buffer 128))
(defonce bass-duration-b (buffer 128))
(defonce bass-notes-b    (buffer 128))

(defsynth woody-beep [duration-bus 0 room 0.5 damp 0.5 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        env (env-gen:ar (env-asr :release 0.25 :sustain 0.8) trig)
        tri (* 0.5 (lf-tri:ar freq))
        sin (sin-osc:ar (* 1 freq))
        sin2 (sin-osc:ar (* 1.01 freq))
        wood (bpf:ar (* (white-noise:ar) (line:kr 5 0 0.02)) freq 0.02)
        src (mix [sin sin2 tri wood])
        src (free-verb src 0.33 room damp)]
    (out:ar out-bus (* amp env (pan2 src (t-rand:kr -1 1 trig))))))

(defsynth deep-saw [freq 100 beat-count-bus 0 offset-bus 0 duration-bus 0 out-bus 0 amp 1 pan 0 room 0.5 damp 0.5]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        saw1 (lf-saw:ar (* 0.5 freq))
        saw2 (lf-saw:ar (* 0.25 freq))
        sin1 (sin-osc freq)
        sin2 (sin-osc (* 1.01 freq))
        src (mix [saw1 saw2 sin1 sin2])
        env (env-gen:ar (env-asr) trig)
        src (lpf:ar src)
        src (free-verb :in src :mix 0.33 :room room :damp damp)]
    (out out-bus (* amp [src src]))))

(def woody (woody-beep :duration-bus duration-b :beat-count-bus timing/beat-count-b :offset-bus score-b :amp 0))

(def deep (deep-saw 100 :duration-bus bass-duration-b :beat-count-bus timing/beat-count-b :offset-bus bass-notes-b :amp 0))

(def score [:F4 :F4 :F4 :F4 :F4 :F4 :F4
            :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4
            :BB4 :BB4 :BB4 :BB4 :BB4 :BB4
            :D#4 :D#4 :D#4])

(def bass-score [:F2 :F2 :G3 :G2 :G3 :BB2 :BB2 :G2 :G2])

(def duration   [1/7])


(def n-score (concat (map #(+ -5 (note %)) score)
                     (map #(+ -5 (note %)) score)
                     (map #(+ -10 (note %)) score)
                     (map #(+ -5 (note %)) score)
                     (map #(+ -1 (note %)) score)))

(buffer-write! bass-duration-b (take 128 (cycle [(/ 1 3.5)])))
(buffer-write! bass-notes-b  (take 128 (cycle (map note bass-score))))
(buffer-write! bass-notes-b (take 128 (cycle (map #(+ -12 (note %)) score))))

(buffer-write! bass-notes-b  (take 128 (cycle (map note [:F2 :G2 :F2]))))

(buffer-write! score-b (take 128 (cycle (map #(+ 0 ( note %)) n-score))))
(buffer-write! score-b (take 128 (cycle (map #(+ 0 ( note %)) score))))

(buffer-write! duration-b  (take 128 (cycle [1/7])))

(ctl woody :amp 6)
(ctl deep :amp 0.8)

(ctl woody :damp 0)
(ctl woody :room 0)

(ctl deep :damp 0)
(ctl deep :room 0)

(kill ps)

(kill woody)
(kill deep)

(stop)
