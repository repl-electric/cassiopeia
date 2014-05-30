(ns cassiopeia.destination.pop
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
;;  (:use dirt)
 (:require [cassiopeia.engine.timing :as time]))

(ctl time/root-s :rate 8)

(defonce coef-b (buffer 128))

(definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                         damp 0.2
                         coef-buf coef-b
                         beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat)
                         notes-buf 0 dur-buf 0
                         mix-rate 0.5]
  (let [cnt (in:kr beat-bus)
        trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        dur (buf-rd:kr 1 dur-buf cnt)
        coef (buf-rd:kr 1 coef-buf cnt)

        freq   (midicps note)
        noize  (* 0.5 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize trg dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        clp (mix [clp
                  (* 1.01 (sin-osc freq (* 2 Math/PI)))
                  (rlpf (saw freq) 900)])

        clp (comb-n clp 0.9 )

        reverb (free-verb clp mix-rate 2 damp)]
    (* amp (env-gen (perc 5 5) :gate trg :time-scale dur) reverb)))

(defonce note-b (buffer 128))
(defonce note-dur-b (buffer 128))
(defonce note1-b (buffer 128))
(defonce note1-dur-b (buffer 128))

(def puck (plucked-string :notes-buf note-b :amp 0.1 :dur-buf note-dur-b :coef-b coef-b :decay 50 :mix-rate 0.2))
(ctl puck :mix-rate 0.3)

(kill plucked-string)

(pattern! coef-b
          (repeat 16 [0.7 0.7 0.7])
          (repeat 8  [0.6 0.6 0.6])
          (repeat 16 [0.7 0.7 0.7])
          (repeat 8  [0.5 0.5 0.5])
          (repeat 16 [0.1 0.1 0.1])
          )

(pattern! note-dur-b [1])

(pattern! note-dur-b
          (repeat 3 [1 1/2 1 1/2]) [1/8 1/8 1/8 1/4]
          (repeat 2 [1 1 1 1])
          (repeat 3 [1 1/2 1 1/2]) [1/8 1/8 1/8 1/4]
          (repeat 2 [1 1 1 1])
          (repeat 4 [1/12 1/12 1 1/12]))

(pattern! note-b (concat (repeat 16  (degrees [1 2 3] :minor :F3))
                         (repeat 8   (degrees [1 2 4] :minor :F3))
                         (repeat 16  (degrees [3 3 5] :minor :F3))
                         (repeat 8   (degrees [1 3 5] :minor :F3))
                         (repeat 16  (degrees [5 4 7] :minor :F3))))

(pattern! note1-dur-b
          [1/2 1/2 1/12 1/12]
          [1/8 1/8 1/8 1/8])

(pattern! note1-b
          (repeat 16 (degrees [1 3] :minor :F3))
          (repeat 8 (degrees  [3 5] :minor :F3))
          (repeat 16 (degrees [5 7] :minor :F3))
          (repeat 8  (degrees [5 3] :minor :F3))
          (repeat 16  (degrees [5 7] :minor :F3)))

(def puck1 (plucked-string :notes-buf note1-b :amp 0.2 :dur-buf note1-dur-b :coef 0.8 :decay 1 :mix-rate 1.0) )

(comment
  (kill plucked-string))

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

(pattern! effects2-seq-buf [1 0 0 0] [1 0 1 1] [0 0 0 0] [1 1 1 1])
(pattern! effects-seq-buf [0 1] (repeat 12 [0]) [1 1])

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.09 :num-steps 8 :buf (buffer-mix-to-mono deep-bass-kick-s)) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.1 :num-steps 8 :buf snare-ghost-s) (range 0 16))))


(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf tom-s) (range 0 16))))

(def two-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.5 :num-steps 8 :buf glitchy-kick-s) (range 0 16))))

(do
  (definst space-flute [freq 880 amp 0.5 attack 0.4 decay 0.5 sustain 0.8 release 1 gate 1 out 0
                        beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) notes-buf 0 dur-buf 0]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          freq (midicps note)

          env  (env-gen (adsr attack decay 0.5 0.3) :gate trg :time-scale dur)
          sig (distort (* env (sin-osc [freq (* 1.01 freq)])))
          sig (mix [(bpf (* 0.01 (pink-noise)) 800) (* amp sig)])

;;          sig (rlpf sig 2000)
;;          sig (pluck sig trg 1.9 1.9 2.0 0.1)
          sig (free-verb sig :mix 0.8 :room 10 :damp 0)
          ]
      sig))
  (defonce note-flute-b (buffer 128))

  (pattern! note-flute-b
            (repeat 16 (degrees [3 3 3] :minor :F4))
            (repeat 8 (degrees  [5 5 5] :minor :F4))
            (repeat 16 (degrees [3 3 3] :minor :F4))
            (repeat 8 (degrees  [7 7 7] :minor :F4))
            (repeat 16 (degrees [8 8 8] :minor :F4)))

  (kill space-flute)

  (def s (space-flute :notes-buf note-flute-b :amp 0.2 :dur-buf note1-dur-b :attack 0.01
                      :beat-bus (:count time/beat-1th)
                      :beat-trg-bus (:beat time/beat-1th)))

  )

(ctl (foundation-root-group) :volume 3)

(definst rize-fall-pad
  [freq 440 t 4 amt 0.3 amp 0.8
   mix-rate 0.8 room-rate 0.5
 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) notes-buf 0 dur-buf 0
   ]
  (let [cnt (in:kr beat-bus)
        trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        dur (buf-rd:kr 1 dur-buf cnt)
        freq (midicps note)

        f-env      (env-gen (perc t t) trg 1 0 1)
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor mix-rate room-rate dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
        (* amp echo)))

(def s (rize-fall-pad :notes-buf note-b :amp 0.9 :dur-buf note1-dur-b
                      :beat-bus (:count time/beat-1th)
                      :beat-trg-bus (:beat time/beat-1th) :attack 0.9))
(ctl s :room-rate 1 :mix-rate 0.8 :amt 0.3 :attack 0.002 :decay 0.0001 :amp 0.2)
;;(stop)

(defonce tonal-notes-b (buffer 256))
(defonce tonal-reverb-b (buffer 256))
(defonce tonal-dur-b (buffer 256))

(pattern! tonal-notes-b
          (repeat 16 (degrees [1 2 1] :minor :F4))
          (repeat 8 (degrees  [1 3 1] :minor :F4))
          (repeat 16 (degrees [3 4 3] :minor :F4))
          (repeat 8 (degrees  [3 5 3] :minor :F4))

          (repeat 16 (degrees [5 7 5] :minor :F4))
          (repeat 8 (degrees  [5 8 5] :minor :F4))
          (repeat 24 (degrees [8 5 8] :minor :F4)))

(pattern! tonal-notes-b
          (repeat 16 0)
          (repeat 8 0)
          (repeat 16 (degrees [3 4 3] :minor :F4))
          (repeat 8 (degrees  [3 5 3] :minor :F4))
          (repeat 16 (degrees [5 7 5] :minor :F4))
          (repeat 8 (degrees  [8 5 8] :minor :F4)))

(pattern! tonal-dur-b (repeat 16 [1 1 1/2 1/2 1/4]))

(do
  (defsynth tonal [amp 1 notes-buf 0
                   beat-trg-bus (:beat time/beat-2th)
                   beat-bus (:count time/beat-2th)
                   dur-buf tonal-dur-b
                   ]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          gate-trig (and (> note 0) trg)

          freq (midicps note)
          src (sum [(sin-osc freq)
                    (saw freq)
                    (blip freq (* 0.5 (sin-osc:kr 0.5)))])
          dly  (/ 1 freq)
          src (pluck src gate-trig dly dly 0.9 0.4)
          src (rlpf src 1000)
          e (env-gen (perc :attack 2 :release 2) :gate gate-trig :time-scale dur)
;;          src (free-verb src 0.7 1 0)
          src (g-verb src 200 8)
          ]
      (out 0 (pan2 (* e amp src)) )))

  (kill tonal)
  (tonal :amp 0.1 :notes-buf tonal-notes-b :reverb-buf tonal-reverb-b)
  )

(stop)

(kill plucked-string)

(comment
  (recording-start "~/Desktop/pop3.wav")
  (recording-stop)

  )
(kill drum-effects-g)
