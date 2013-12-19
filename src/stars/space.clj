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
-
(tick)
(ding :dur 0.5)


(def note-offset-b (buffer 128))
(def duration-b (buffer 128))

(def note-offsets [:F4 :F4 :F4 :F4 :F4 :F4 :F4
                   :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4
                   :BB4 :BB4 :BB4 :BB4 :BB4 :BB4
                   :D#4 :D#4 :D#4])

(def duration     [1/7])

(def score (concat
            (map #(+  0  (note %)) note-offsets)
            (map #(+ -5  (note %)) note-offsets)
            (map #(+ -10   (note %)) note-offsets)
            (map #(+ -5 (note %)) note-offsets)
            (map #(+ -1   (note %)) note-offsets)))

(buffer-write! note-offset-b (take 128 score))

(buffer-write! duration-b (take 128 (cycle duration)))

(ctl timing/root-s :rate 100)

(do
  (defsynth woody-beep [duration-bus 0 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0]
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
          src (free-verb src)]
      (out:ar out-bus (* amp env (pan2 src)))))

    (kill woody-beep)

    (def w  (woody-beep :duration-bus duration-b :beat-count-bus timing/beat-count-b :offset-bus note-offset-b :amp 7)))

(defsynth deep-saw [freq 100 beat-count-bus 0 offset-bus 0 duration-bus 0 out-bus 0 amp 1 pan 0]
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
        src (free-verb src 0.33 1 1)]
    (out out-bus (* amp [src src]))))

(def bass-duration-b (buffer 32))
(def bass-notes-b    (buffer 32))

(buffer-write! bass-duration-b (take 32 (cycle [1/14])))
(buffer-write! bass-notes-b (take 32 (cycle (map note [:F2 :F2 :G3 :G2 :G3 :BB2 :BB2 :G2 :G2]))))
(buffer-write! bass-notes-b (take 32 (cycle (map note [:G4 :F#4 :E4 :D#4 :B3 :G3 :F3]))))

(def ps (deep-saw 100 :duration-bus bass-duration-b :beat-count-bus timing/beat-count-b :offset-bus note-offset-b :amp 1))

(ctl ps :freq 200)

(stop)
(kill deep-saw)
