(ns cassiopeia.destination.pop
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
 (:require [cassiopeia.engine.timing :as time]))

(ctl time/root-s :rate 8)

(definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                         beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) notes-buf 0 dur-buf 0]
  (let [cnt (in:kr beat-bus)
        trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        dur (buf-rd:kr 1 dur-buf cnt)

        freq   (midicps note)
        noize  (* 0.5 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize trg dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.5 1.99 0.2)]
    (* amp (env-gen (perc 1.0 dur) :gate trg) reverb)))

(defonce note-b (buffer 128))
(defonce note-dur-b (buffer 128))

(def puck (plucked-string :notes-buf note-b :amp 0.2 :dur-buf note-dur-b :coef 0.8 :decay 50))
(ctl puck :coef 0.3 :decay 70)

(pattern! note-dur-b
          (repeat 3 [1/2 1/2 1/2 1/2])
                    [1/8 1/8 1/8 1/8])

(pattern! note-b
          (repeat 16 (degrees [1 3] :minor :F3))
          (repeat 16 (degrees [3 5] :minor :F3))
          (repeat 16 (degrees [5 7] :minor :F3)))
