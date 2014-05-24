(ns cassiopeia.destination.pop
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
;;  (:use dirt)
 (:require [cassiopeia.engine.timing :as time]))

(ctl time/root-s :rate 8)

(definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                         damp 0.2
                         beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) notes-buf 0 dur-buf 0
                         mix-rate 0.5]
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
        reverb (free-verb clp mix-rate (+ 1.0 (* 0.5 (sin-osc:kr 20))) damp)]
    (* amp (env-gen (perc 2.0 dur) :gate trg) reverb)))

(defonce note-b (buffer 128))
(defonce note-dur-b (buffer 128))

(def puck (plucked-string :notes-buf note-b :amp 0.2 :dur-buf note-dur-b :coef 0.8 :decay 50))
(ctl puck :coef 0.8 :decay 80)
(ctl puck :mix-rate 0.1)

(pattern! note-dur-b (repeat 3 [1 1/2 1 1/2]) [1/8 1/8 1/8 1/4])

(pattern! note-b (repeat 16  (degrees [1 3] :minor :F3))
                 (repeat 8   (degrees [1 4] :minor :F3))
                 (repeat 16  (degrees [3 5] :minor :F3))
                 (repeat 8   (degrees [1 5] :minor :F3))
                 (repeat 16  (degrees [5 7] :minor :F3)))


(defonce note1-b (buffer 128))
(defonce note1-dur-b (buffer 128))

(pattern! note1-dur-b [1])

(pattern! note1-b
          (repeat 16 (degrees [1 3] :minor :F3))
          (repeat 8 (degrees  [3 5] :minor :F3))
          (repeat 16 (degrees [5 7] :minor :F3))
          (repeat 8  (degrees [5 3] :minor :F3))
          (repeat 16  (degrees [5 7] :minor :F3)))

(def puck1 (plucked-string :notes-buf note1-b :amp 0.2 :dur-buf note1-dur-b :coef 0.8 :decay 50))
(ctl puck1 :coef 0.8 :decay 20 :mix-rate 0.9)

(comment
  (kill plucked-string))

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 96 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

(pattern! effects2-seq-buf [1 0 0 0] [1 0 1 1] [0 0 0 0] [1 1 1 1])



(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf deep-bass-kick-s) (range 0 16)))) (pattern! (degrees [1 0 0 0 0 1] ))
(kill drum-effects-g)

(definst space-flute [freq 880 amp 0.5 attack 0.4 decay 0.5 sustain 0.8 release 1 gate 1 out 0
                       beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) notes-buf 0 dur-buf 0]
  (let [cnt (in:kr beat-bus)
        trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        dur (buf-rd:kr 1 dur-buf cnt)
        freq (midicps note)

        env  (env-gen (adsr attack decay 3 3) :gate trg :time-scale dur)

        ;;mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        ;;mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq freq])))
        sig (* amp sig)
        sig (free-verb sig :mix 0.9 :room 2 :damp 1)
        ]
    sig))
(defonce note-flute-b (buffer 128))

(pattern! note-flute-b (degrees [3 3 3 3 -1 1 1 1 1] :minor :F3))
(def s (space-flute :notes-buf note-flute-b :amp 0.8 :dur-buf note1-dur-b :attack 0.01
                     :beat-bus (:count time/beat-2th)
                     :beat-trg-bus (:beat time/beat-2th)))

(ctl (foundation-root-group) :volume 1)
