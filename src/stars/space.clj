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

(tick)
(ding :dur 0.5)

(defonce note-offset-b (buffer 8))
(defonce duration-b (buffer 8))

(def note-offsets [0 0 7 0 7 10 7 2])
(def duration [0.4 0.4 0.2 0.2 0.15 0.5 0.1 0.3])

(buffer-write! note-offset-b note-offsets)
(buffer-write! duration-b duration)

(defsynth timed-playback [beat-count-bus 0
                          out-bus 0
                          offset-bus 0
                          duration-bus 0
                          amp 1]
  (let [cnt (in:kr beat-count-bus)
        freq (buf-rd:kr 1 offset-bus cnt)
        freq (midicps (+ 60 freq))
        dur (buf-rd:kr 1 duration-bus cnt)

        env  (env-gen:ar (env-perc) :time-scale dur)
        snd (* (sin-osc:ar freq) amp 1)]
    (out out-bus snd)))

(timed-playback :beat-count-bus timing/beat-count-b
                :offset-bus note-offset-b
                :duration-bus duration-b)

(kill timed-playback)
