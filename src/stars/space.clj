(ns stars.space
  (:require [stars.engine.timing :as timing])
  (:use overtone.live))

(defsynth ding [freq 880 dur 0.2 level 0.25 pan 0.0 out-bus 0]
  (let [amp  (env-gen:ar (env-perc) :action FREE :time-scale dur)
        snd (* (sin-osc:ar freq ) amp level)]
    (out out-bus (pan2:ar snd pan))))

(defsynth tick [freq 880 dur 0.1 level 0.25 pan 0.0 out-bus 0]
  (let [amp (env-gen (env-perc) :action FREE :time-scale dur)
        snd (lpf:ar (white-noise:ar) freq)]
    (out out-bus (pan2:ar (* snd amp level) pan))))

(def notes-b    (buffer 32))
(def duration-b (buffer 32))

(def score [:F4 :F4 :F4 :F4 :F4 :F4 :F4
            :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4
            :BB4 :BB4 :BB4 :BB4 :BB4 :BB4
            :D#4 :D#4 :D#4])

(def duration [1/7])

(buffer-write! note-offset-b (take 32 (cycle
                                       (map (fn [n] (+ 0 n))
                                            (map note score)))))

(buffer-write! duration-b (take 32 (cycle duration)))

(ctl timing/root-s :rate 100)

(defsynth bouncing-beep [duration-bus 0 beat-count-bus 0 notes-bus 0 amp 1 out-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 notes-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)
        env (env-gen:ar (env-asr :release 0.25 :sustain 0.8) trig)
        tri (* 0.5 (lf-tri:ar freq))
        sin (sin-osc:ar freq)
        src (mix [sin tri])
        src (free-verb src)]
    (out:ar out-bus (* amp env (pan2 src)))))

(kill seq-synth)

(bouncing-beep :duration-bus duration-b
               :beat-count-bus timing/beat-count-b
               :offset-bus notes-b
               :amp 1)
