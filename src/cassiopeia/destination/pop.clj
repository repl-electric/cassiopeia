(ns cassiopeia.destination.pop
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  (:use cassiopeia.dirt)
;;  (:use dirt)
  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]))

(ctl time/root-s :rate 8.)

(defonce coef-b (buffer 128))

(do
  (definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                           damp 0.2
                           coef-buf coef-b
                           beat-bus (:count time/beat-1th) beat-trg-bus (:beat time/beat-1th)
                           notes-buf 0 dur-buf 0
                           mix-rate 0.5]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          coef (buf-rd:kr 1 coef-buf cnt)

          freq   (midicps note)
          noize  (* (lf-tri freq (sin-osc:kr 0.5)))
          dly    (/ 1.0 freq)
          plk    (pluck noize trg dly dly decay coef)
          dist   (distort plk)
          filt   (rlpf dist (* 12 freq) 0.6)
          clp    (clip2 filt 0.8)
          clp (mix [clp
                    (* 1.01 (sin-osc freq (* 2 Math/PI)))
                    (rlpf (saw freq) 1200)])

          clp (comb-n clp 0.9)
          reverb clp

          ;;reverb (g-verb clp 400 2.0 0.0 0.1 0.3 0.2 0.5 0.1 400)
          reverb (g-verb clp 250 20 0)
          ]
      (pan2 (* amp (env-gen (perc 0.03 0.2) :gate trg :time-scale dur) reverb))))

  (defonce note-b (buffer 128))
  (defonce note-dur-b (buffer 128))
  (defonce note1-b (buffer 128))
  (defonce note1-dur-b (buffer 128))

(kill plucked-string)

  (def puck (plucked-string :notes-buf note-b :amp 0.05 :dur-buf note-dur-b :coef-b coef-b :decay 50 :mix-rate 0.3))
  (ctl puck :decay 90 :amp 0.04)
  )

(pattern! coef-b
          (repeat 6 [0.5 0.5 0.5 0.4])
          (repeat 6 [0.4 0.4 0.4 0.3])
          (repeat 6 [0.5 0.5 0.5 0.4])
          (repeat 6 [0.3 0.3 0.2 0.2])
          )

(pattern! note-dur-b [4 4 4 4 4 2 2 2 2 2 1 1 1])

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


(pattern! note-b (map #(- % 0)
                      (flatten (concat
                                (repeat 16  (degrees [1 3 1] :minor :F3))
                                (repeat 8   (degrees [4 3 4] :minor :F3))
                                (repeat 16  (degrees [1 3 1] :minor :F3))
                                (repeat 8   (degrees [4 3 4] :minor :F3))
                                (repeat 16  (degrees [1 3 1] :minor :F3))))))

(pattern! note1-dur-b
          [1/2 1/2 1/12 1/12]
          [1/8 1/8 1/8 1/8])

(pattern! note-dur-b [1. 1. 1.5])

(pattern! note-b
          (repeat 8 (degrees [1 0 0] :major :F3))
          (repeat 8 (degrees [4 0 0] :major :F3))
          (repeat 8 (degrees [1 0 0] :major :F3))
          (repeat 8 (degrees [4 0 0] :major :F3))
         ;;          (repeat 8 (degrees [1 3 1] :major :F3))
)

(pattern! note-b
          (repeat 8 (degrees [1 0 0] :major :F3))
          (repeat 8 (degrees [4 0 0] :major :F3))
          (repeat 8 (degrees [1 0 0] :major :F3))
          (repeat 8 (degrees [4 0 0] :major :F3))

          (repeat 8 (degrees [3 0 0] :major :F3))
          (repeat 8 (degrees [5 0 0] :major :F3))
          (repeat 8 (degrees [3 0 0] :major :F3))
          (repeat 8 (degrees [6 0 0] :major :F3))


          (repeat 8 (degrees [1 0 0] :major :F4))
          (repeat 8 (degrees [4 0 0] :major :F4))
          (repeat 8 (degrees [1 0 0] :major :F4))
          (repeat 8 (degrees [4 0 0] :major :F4))

          (repeat 8 (degrees [3 0 0] :major :F4))
          (repeat 8 (degrees [5 0 0] :major :F4))
          (repeat 8 (degrees [3 0 0] :major :F4))
          (repeat 8 (degrees [6 0 0] :major :F4))




          ;;          (repeat 8 (degrees [1 3 1] :major :F3))
          )



(defonce coef2-b (buffer 128))
(pattern! coef2-b 0.8)
(def puck1 (plucked-string :notes-buf note-b :amp 0.2 :dur-buf note1-dur-b :coef-b coef2-b :decay 0.001 :mix-rate 1.0) )

(fadeout puck1)

(comment
  (kill plucked-string))

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

(pattern! effects2-seq-buf (repeat 15 0) [1])
(pattern! effects-seq-buf (repeat 15 0)  [1 1 1 1 1 1])

(pattern! effects2-seq-buf
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 1])

(pattern! effects-seq-buf
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [1 0 0 0 0 0 0 0])

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.3 :num-steps 16 :buf (buffer-mix-to-mono deep-bass-kick-s)) (range 0 16))))
(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.04 :num-steps 16 :buf bell-s) (range 0 16))))


(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.8 :num-steps 8 :buf snare-ghost-s) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.02 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))
(def two-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.02 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.2 :num-steps 8 :buf beep-s) (range 0 48))))

(def two-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.03 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16))))

(kill drum-effects-g)
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
          sig (mix [(* amp sig)
                    ;;(lpf (lf-saw (* 0.25 freq)) 100)
                    (bpf (* 0.01 (pink-noise)) 800)])

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

;;(ctl (foundation-output-group) :master-volume 1)

(do
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
          distort    (/ (* (+ 2 k) signal) (+ 2 (* k (abs signal))))
          gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
          compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
          dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
          reverb     (free-verb compressor mix-rate room-rate dampener)
          echo       (comb-n reverb 0.4 0.3 0.5)]
      (* amp echo)))

;;  (kill s)

  (def s (rize-fall-pad :notes-buf note-b :amp 0.0 :dur-buf note1-dur-b
                        :beat-bus (:count time/beat-1th)
                        :beat-trg-bus (:beat time/beat-1th) :attack 0.9)))
(ctl s :room-rate 1 :mix-rate 0.8 :amt 0.3 :attack 0.002 :decay 0.001 :amp 0.0)
(ctl s :amt 0.6 :attack 0.1 :decay 0.8 :mix-rate 0.5)
(fadein s 0.1 0.01)

;;(stop)

(defonce tonal-notes-b (buffer 256))
(defonce tonal-reverb-b (buffer 256))
(defonce tonal-dur-b (buffer 256))
(defonce tonal-amp-b (buffer 256))

(pattern! tonal-notes-b
          (repeat 16 (degrees [1 2 1] :minor :F4))
          (repeat 8 (degrees  [1 3 1] :minor :F4))
          (repeat 16 (degrees [3 4 3] :minor :F4))
          (repeat 8 (degrees  [3 5 3] :minor :F4))

          (repeat 16 (degrees [5 7 5] :minor :F4))
          (repeat 8 (degrees  [5 8 5] :minor :F4))
          (repeat 24 (degrees [8 5 8] :minor :F4)))

(pattern! tonal-notes-b
          (repeat 8  (degrees  [2 0 0] :minor :F4)) (repeat 8 (degrees  [2 0 0] :minor :F3))
          (repeat 8  (degrees  [3 0 0] :minor :F4)) (repeat 8 (degrees  [3 0 0] :minor :F3))
          (repeat 8  (degrees  [4 0 0] :minor :F4)) (repeat 8 (degrees  [4 0 0] :minor :F3))
          (repeat 8  (degrees  [3 0 0] :minor :F4)) (repeat 8 (degrees  [3 0 0] :minor :F3))
          (repeat 8  (degrees  [2 0 0] :minor :F4)) (repeat 8 (degrees  [2 0 0] :minor :F3))
          (repeat 8  (degrees  [5 0 0] :minor :F4)) (repeat 8 (degrees  [5 0 0] :minor :F3)))

(pattern! tonal-notes-b
          (repeat 8  (degrees  [1 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [5 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [3 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [4 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [3 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [2 0 0] :minor :F4)) (repeat 8 [0])
          (repeat 8  (degrees  [8 0 0] :minor :F4)) (repeat 8 [0]))

(pattern! tonal-notes-b
          (repeat 6  (degrees  [1 1 0]  :major :F4)) (repeat 8 0)
          (repeat 6  (degrees  [3 3 2]  :major :F4)) (repeat 8 0)
          (repeat 6  (degrees  [5 5 4]  :major :F4)) (repeat 8 0)
          (repeat 6  (degrees  [3 3 2]  :major :F4)) (repeat 8 0) )

(pattern! tonal-dur-b (repeat 16 [1/2 1/2 1/2 1/2 1]))

(pattern! tonal-amp-b (repeat 3 [0.1]) (repeat 8 0.05))

(do
  (defsynth tonal [amp 1 notes-buf 0
                   beat-trg-bus (:beat time/beat-1th)
                   beat-bus (:count time/beat-1th)
                   dur-buf tonal-dur-b
                   amp-buf tonal-amp-b
                   room-size 200
                   rev-time 8
                   damping 0.5
                   offset 0
                   ]
    (let [cnt (+ offset (in:kr beat-bus))
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          b-amp (buf-rd:kr 1 amp-buf cnt)
          gate-trig (and (> note 0) trg)

          freq (midicps note)
          src (sum [(sin-osc freq)
                    (saw freq)
                    (blip freq (* 0.5 (sin-osc:kr 0.5)))])
          dly  (/ 1 freq)
          ;;src (pluck src gate-trig dly dly 0.9 0.4)
          src (sin-osc freq)
;;          src (rlpf src 1000)
          e (env-gen (adsr :attack 1.0 :decay 1.0 :level 1.0 :release 10) :gate gate-trig :time-scale dur)
;;          src (free-verb src 0.7 1 0)
          src (g-verb src room-size rev-time damping)
          ]
      (out 0 (pan2 (* e (* amp b-amp) src)) )))


    (kill tonal)


  (defonce  tonal-notes2-b (buffer 256))
  (defonce  tonal-reverb2-b (buffer 256))
  (defonce tonal-amp2-b (buffer 256))
  (defonce tonal-dur2-b (buffer 256))

  (pattern! tonal-notes2-b (degrees [0 0 6] :minor :F4))
  (pattern! tonal-reverb2-b [0.6])
  (pattern! tonal-dur2-b [2])

  (pattern! tonal-notes2-b
            (reverse (flatten (concat (repeat 8  (degrees  [0 0 1] :minor :F3))
                                      (repeat 8  (degrees  [0 0 5] :minor :F3))
                                      (repeat 8  (degrees  [0 0 3] :minor :F3))
                                      (repeat 8  (degrees  [0 0 4] :minor :F3))
                                      (repeat 8  (degrees  [0 0 3] :minor :F3))
                                      (repeat 8  (degrees  [0 0 2] :minor :F3))
                                      (repeat 8  (degrees  [0 0 8] :minor :F3))))))


  (pattern! tonal-notes2-b
            (repeat 8  (degrees  [2 1 1] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [5 4 4] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [3 2 2] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [4 3 3] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [3 2 2] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [5 4 4] :minor :F4)) (repeat 8 [0])
            (repeat 8  (degrees  [8 7 7] :minor :F4)) (repeat 8 [0]))

  (pattern! tonal-amp2-b (repeat 3 [0.3 0.1 0.1 0.1 0.08 0.07 0.07 0.05]) (repeat 8 0.04))

  (kill tonal)

  (comment
    (def tonal2 (tonal :amp 0.1 :notes-buf tonal-notes2-b :reverb-buf tonal-reverb2-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)
                       :amp-buf tonal-amp2-b :dur-buf tonal-dur2-b :rev-time 2 :room-rate 200 :damping 0.0))

    (def tonal2 (tonal :amp 0.5 :notes-buf tonal-notes2-b :reverb-buf tonal-reverb2-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)
                       :amp-buf tonal-amp2-b :dur-buf tonal-dur2-b :rev-time 2 :room-rate 200 :damping 0.0 :offset 2)))


  (def tone (tonal :amp 0.6 :notes-buf tonal-notes-b :reverb-buf tonal-reverb-b
                   :rev-time 0 :room-rate 0 :dur-buf tonal-dur-b))
  )
(stop)
;;(fadeout tonal)

(comment
  (recording-start "~/Desktop/pop7.wav")
  (recording-stop)
  (stop)
  )

;;(fadeout-master)
(echoey-buf one-moment-please-s :amp 0.1)
(echoey-buf beep-s :amp 0.01)
(echoey-buf goodbye-s :amp 0.2)

(spacy beep-s)

;;(on-beat-trigger 96 #(do (echoey-buf one-moment-please-s :amp 0.02)))
;;(on-beat-trigger 96 #(do (echoey-buf afraid-s :amp 0.009)))
;;(remove-all-beat-triggers)

;;(on-beat-trigger 128 #(do (echoey-buf (dirt :arp 0) :amp 0.09)))
;;(on-beat-trigger 32  #(do (echoey-buf (dirt :moog 7) :amp 0.09)))
;;(on-beat-trigger 24  #(do (echoey-buf (dirt :wind 0) :amp 0.1)))

;;(on-beat-trigger 128 #(do (echoey-buf (dirt :crows 1) :amp 0.19)))

;;(remove-all-beat-triggers)

;;{CombL.ar (In.ar (8).tanh/8,1,1,8)!2}.play;Pbind(\amp,8,\dur,1/4,\degree,Pseq(List.fib(32)%(List.fib(64)%12),inf),\out,8).play//#SuperCollider

(do
  (definst sharp-twang [notes-buf 0 amp 1
                        beat-trg-bus (:beat time/beat-2th)
                        beat-bus (:count time/beat-2th)
                        dur-buf 0
                        attack-buf 0
                        release-buf 0
                        amp-buf 0
                        overtones 1.5
                        ]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          freq (midicps note)
          dur (buf-rd:kr 1 dur-buf cnt)
          attack (buf-rd:kr 1 attack-buf cnt)
          release (buf-rd:kr 1 release-buf cnt)
          amp-rate (buf-rd:kr 1 amp-buf cnt)

          gate-trig (and (> note 0) trg)

          src  (blip freq (+ overtones (* 0.5 (sin-osc:kr 20))))
          o1 (comb-l:ar (/ (tanh src) 8) 1 1 8)
          o2 (comb-l:ar (/ (tanh src) 8) 1 1 8)
          o1 (free-verb o1)
          o2 (free-verb o2)

          e (env-gen (adsr :attack attack :release release :decay 3) :time-scale dur :gate gate-trig)
          ]
      (* e (* amp-rate amp) (pan2 [o1 o2] (sin-osc:kr 5))))
    )

  (kill sharp-twang)

  (defonce twang-notes-buf (buffer 256))
  (defonce twang-dur-buf (buffer 256))
  (defonce twang-attack-buf (buffer 256))
  (defonce twang-release-buf (buffer 256))
  (defonce twang-amp-buf (buffer 256))

  (sharp-twang :notes-buf twang-notes-buf :amp 4 :dur-buf twang-dur-buf
               :attack-buf twang-attack-buf :release-buf twang-release-buf
               :amp-buf twang-amp-buf
               )

  (pattern! twang-release-buf [0.8  0.8   0.8   4   4   4   4   4   4   4   4   4   4])
  (pattern! twang-attack-buf  [0.08 0.08 0.08 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3])
  (pattern! twang-dur-buf [2 2 2 1/2 1/2 1/2 1 1 1 1 1 1/2 1/2])
  (pattern! twang-amp-buf
            (repeat 3 [1 1 1 1/2 1/2 1/2 1 1 1 1 1 1/2])
            [1.2 1.2 1.2 1/2 1/2 1/2 1 1 1 1 1 1/2])

  (pattern! twang-notes-buf
            (degrees [1 5 3 0 3 0 3 2] :major :F4) [0 0 0 0]
            (degrees [3 5 4 0 4 0 4 3] :major :F4) [0 0 0 0]
            (degrees [5 7 5 0 5 0 5 3] :major :F4) [0 0 0 0]
            (degrees [5 7 6 0 5 0 5 3] :major :F4) [0 0 0 0]
            )

  (defonce twang-notes2-buf (buffer 256))
  (defonce twang-dur2-buf (buffer 256))
  (defonce twang-attack2-buf (buffer 256))
  (defonce twang-release2-buf (buffer 256))
  (defonce twang-amp2-buf (buffer 256))

  (kill sharp-twang)

  (sharp-twang :notes-buf twang-notes2-buf :amp 5 :dur-buf twang-dur2-buf
               :attack-buf twang-attack2-buf :release-buf twang-release2-buf
               :amp-buf twang-amp2-buf :beat-trg-bus (:beat time/beat-2th) :beat-bus  (:count time/beat-2th))

  (pattern! twang-release2-buf [4])
  (pattern! twang-attack2-buf  [0.01])
  (pattern! twang-dur2-buf [5])
  (pattern! twang-amp2-buf (repeat 3 [0.4]))

  (pattern! twang-notes2-buf
            (repeat 1 (concat (repeat 1 (degrees [1] :major :F3)) [0 0 0 0 0 0 0]))
            ;;(repeat 2 [0 0 0 0 0 0 0 0])
            (repeat 1 (concat (repeat 1 (degrees [3] :major :F3)) [0 0 0 0 0 0 0]))
            (repeat 1 (concat (repeat 1 (degrees [5] :major :F3)) [0 0 0 0 0 0 0]))
            (repeat 1 (concat (repeat 1 (degrees [3] :major :F3)) [0 0 0 0 0 0 0]))

;;            (repeat 1 (degrees [2 1 0] :major :F3)) [0 0 0 0 0] (repeat 2 [0 0 0 0 0 0 0 0])
  ;;          (repeat 1 (degrees [3 2 0] :major :F3)) [0 0 0 0 0] (repeat 2 [0 0 0 0 0 0 0 0])
    ;;        (repeat 1 (degrees [2 1 0] :major :F3)) [0 0 0 0 0] (repeat 2 [0 0 0 0 0 0 0 0])
            )


  (pattern! twang-notes2-buf
            (degrees [3 7 5 0 0 0 0 0] :major :F3) [0 0 0 0]
            (degrees [5 7 5 0 0 0 0 0] :major :F3) [0 0 0 0]
            (degrees [7 8 7 0 0 0 0 0] :major :F3) [0 0 0 0]
            (degrees [7 8 7 0 0 0 0 0] :major :F3) [0 0 0 0]

;;            (degrees [1 5 3 0 3 0 3 2] :major :F3) [0 0 0 0]
  ;;          (degrees [3 2 3 0 3 0 3 2] :major :F3) [0 0 0 0]
    ;;        (degrees [6 7 6 0 5 0 5 3] :major :F3) [0 0 0 0]
      ;;      (degrees [7 8 7 0 5 0 5 4] :major :F3) [0 0 0 0]
            )


  (pattern! twang-attack-buf  [0.08 0.08 0.08 0.08 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3])
  (pattern! twang-notes-buf
            [0 0 0] (degrees [1] :major :F4) [0 0 0 0 0 0 0 0]
            [0 0 0] (degrees [3] :major :F4) [0 0 0 0 0 0 0 0]
            [0 0 0] (degrees [3] :major :F4) [0 0 0 0 0 0 0 0]
            [0 0 0] (degrees [5] :major :F4) [0 0 0 0 0 0 0 0])

  (comment
    (pattern! twang-notes-buf
              (degrees [0 0 0 0 0 0 0 3 4 6 4 0 3 2 0 0 0 0 1 6 0 1 0 0 0 0 0 0 4 0 0 3 1 1 2 0 0 3 3 0 0 3 4 0 1 3 0 0 0 0 1 0 1 7 0 0 5 6 3 0 4 0 9 0] :major :F4)))
  )

(stop)

(ctl (foundation-output-group) :master-volume 2)


(do
  (definst sawer [freq 300
                  notes-buf 0 amp 1
                  beat-trg-bus (:beat time/beat-2th)
                  beat-bus (:count time/beat-2th)]

    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          freq (midicps note)
          gate-trig (and (> note 0) trg)

          src (bpf (lf-tri freq) 2000)
          src (comb-l src 2 2 10)
          src (g-verb src :roomsize 200 :revtime 120 :damping 0.5)
          e (env-gen (adsr :attack 0.2 :decay 2 :sustain 2 :release 2) :gate gate-trig)]
      (pan2 (* e amp src))))

  (kill sawer)
  (defonce sawer-notes-buf (buffer 256))

  (sawer :notes-buf sawer-notes-buf :amp 0.2)

  (pattern! sawer-notes-buf
            [0 0 0 0 0 0 0] (degrees [3] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [1] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [5] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [7] :major :F4) [0 0 0 0]

            [0 0 0 0 0 0 0] (degrees [5] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [3] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [6] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [7] :major :F4) [0 0 0 0])


  )

(ctl sawer :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th) :amp 0.4)

(stop)

(require '[clojure.math.numeric-tower :as math])

(do
  (definst zip-zop [amp 1]
    (let [sins
          (map (fn [i]
                 (sin-osc:ar
                  (pow
                   (+ 1 (sin-osc:ar i))
                   (sin-osc:ar
                    (pow 2 (* (sin-osc:ar (/ i 500))
                              (- 9 i)
                              (lin-exp (sin-osc:ar (* 9 i)) 90 (pow 2 (* (sin-osc:ar (/ i 20)) 800))))))))
                 )
               (range 300 400))]
      (* amp (/ (splay:ar sins) 4))))

  (kill zip-zop)
  (def z (zip-zop :amp 1.0))
)

(stop)

(do
  (definst wow []
    (g-verb:ar
     (moog-ff:ar (* 0.3 (clip-noise))
                 (+ 100 (* 300 (lf-par:kr [(rand 0.3) (rand 0.3)] 0)))) 9 9 1))

  (kill wow)
  (wow))
(pb)

(stop)


(do
  (definst slobber [amp 1
                beat-trg-bus (:beat time/beat-1th)
                beat-bus      (:count time/beat-1th)]
    (let [trg (in:kr beat-trg-bus)

          limit 99
          freqs [4 0.5 8 16]
          src1 (+ 12 (* limit (blip:ar freqs (+ limit (* limit (lf-saw:ar (/ 1 freqs) 0))))))
          rq (+ 0.5 (* 0.5 (sin-osc:ar freqs freqs)))
          note (rlpf:ar src1 limit rq)
          freq (midicps note)
          src (formant:ar freq limit)
          src (splay:ar src)
          e (env-gen (perc) :gate trg)
          ]
      (pan2 (* e amp src) (line:kr -1.0 1.0 32 :action FREE))))
  (kill slobber)
  (slobber :amp 0.05)
)

(do
  (definst echoey-twang [amp 1
                         beat-trg-bus (:beat time/beat-1th)
                         beat-bus      (:count time/beat-1th)
                         notes-buf 0]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          freq (midicps note)
          gate-trg (and (> note 0) trg)

          a [1 2 4 5]
          e (env-gen (adsr) :gate gate-trg)

          src (lag (blip:ar a) (* 1 (+ 1 (lf-saw:ar (/ 1 (+ 2.25 a)) (/ 2 a)))))
          src (splay:ar  (* (sin-osc:ar freq (* 99 (blip:ar 2 (sin-osc 99)))) src))
          src (g-verb:ar src 99 6 0.7)]
      (* e  amp src)))

  (defonce echo-note-b (buffer 256))
  (kill echoey-twang)

  (echoey-twang :amp 1 :notes-buf echo-note-b)
  (pattern! echo-note-b (degrees [8] :major :F2))
)
(stop)
(remove-all-beat-triggers)

(comment

  (recording-start "~/Desktop/pop.wav")
  (recording-stop)
  (stop)
  (remove-all-beat-triggers)
  )

