(ns cassiopeia.destination.pop
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  (:use cassiopeia.dirt)
  (:require [cassiopeia.engine.timing :as time]
            [clojure.math.numeric-tower :as math]
            [overtone.studio.fx :as fx]))

(defonce n1 (buffer 256))
(defonce n2 (buffer 256))
(defonce n3 (buffer 256))
(defonce n4 (buffer 256))

(ctl time/root-s :rate 8.)

(defonce coef-b (buffer 128))

(do
  (definst plucked-string [amp 0.8 decay 30 coef 0.3 gate 1
                           release 0.2
                           attack 0.03
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
      (pan2 (* amp (env-gen (perc attack release) :gate trg :time-scale dur) reverb))))

  (defonce note-b (buffer 128))
  (defonce note-dur-b (buffer 128))
  (defonce note1-b (buffer 128))
  (defonce note1-dur-b (buffer 128))

  (kill plucked-string)


  (defonce puck-notes1-b (buffer 256))
  (defonce puck-notes2-b (buffer 256))
  (defonce puck-notes3-b (buffer 256))

  (do
    (def puck (plucked-string :notes-buf puck-notes1-b :amp 0.04 :dur-buf note-dur-b :coef-b coef-b :decay 90 :mix-rate 0.3 :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th)  :attack 0.9))
    (def puck (plucked-string :notes-buf puck-notes2-b :amp 0.04 :dur-buf note-dur-b :coef-b coef-b :decay 90 :mix-rate 0.3 :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th)  :attack 0.9))
    (def puck (plucked-string :notes-buf puck-notes3-b :amp 0.04 :dur-buf note-dur-b :coef-b coef-b :decay 90 :mix-rate 0.3 :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th))))

  (map find-note-name (chord-degree :v :F3 :minor))
  :C4 :Eb4 :G4 :Bb4

  (on-beat-trigger 128 (fn [] (ping-chords)))
(remove-all-beat-triggers)

(defn ping-chords []
  (pattern! note-dur-b (shuffle [1/2 1/2 2 2]) (repeat 7 [1]))
  (let [_ [0 0 0]
        [F21 F22 F23 F24 F25 F26 F27] (map #(chord-degree %1 :F2 :major) [:i :ii :iii :iv :v :vi :vii])
        [F31 F32 F33 F34 F35 F36 F37] (map #(chord-degree %1 :F3 :major) [:i :ii :iii :iv :v :vi :vii])
        [F41 F42 F43 F44 F45 F46 F47] (map #(chord-degree %1 :F4 :minor) [:i :ii :iii :iv :v :vi :vii])]
    (let [start (choose [F31 F33])
          chord-pat
          (concat
           (repeat 1 [start start])
           (repeat 2 [F33])
           (repeat 2 [F31])
           (repeat 2 [F33])
           (repeat 2 [F36])
           (repeat 2 [(choose [F32 F34])])
           (repeat 2 [(choose [F35 F37])])
           (repeat 2 [F31]))

          chord-bufs (shuffle [puck-notes1-b puck-notes2-b puck-notes3-b])]
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))
  )

(def puck (plucked-string :notes-buf note-b :amp 0.04 :dur-buf note-dur-b :coef-b coef-b :decay 90 :mix-rate 0.3))


;;  (ctl puck :notes-buf twang-notes-buf)
  (ctl puck :decay 90 :amp 0.04)

  (ctl puck :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :release 0.2 :attack 0.03 :amp 0.08)
  (ctl puck :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :release 0.9 :attack 0.9 :amp 0.07)

  ;; (n-overtime! puck :release 2 0.2 0.01)
)

(pattern! coef-b (repeat (* 3 4 6) [0.2]) [0.1 0.1 0.1 0.1])

(pattern! coef-b
          (repeat 6 [0.6 0.6 0.6 0.6])
          (repeat 6 [0.4 0.4 0.4 0.4])
          (repeat 6 [0.5 0.5 0.5 0.4])
          (repeat 6 [0.2 0.2 0.2 0.1]))

(pattern! note-dur-b [4 4 4 4 4 2 2 2 2 2 1 1 1])

(pattern! note-dur-b
          (repeat 3 [1 1/2 1 1/2]) [1/8 1/8 1/8 1/4]
          (repeat 2 [1 1 1 1])
          (repeat 3 [1 1/2 1 1/2]) [1/8 1/8 1/8 1/4]
          (repeat 2 [1 1 1 1])
          (repeat 4 [1/12 1/12 1 1/12]))

(pattern! note-b (concat (repeat 16  (degrees [1 2 3] :major :F3))
                         (repeat 8   (degrees [1 2 4] :major :F3))
                         (repeat 16  (degrees [3 3 5] :major :F3))
                         (repeat 8   (degrees [1 3 5] :major :F3))
                         (repeat 16  (degrees [5 4 7] :major :F3))))

(pattern! note-b (map #(+ % 0)
                      (flatten (concat
                                (repeat 16  (degrees [1 3 1] :major :F3))
                                (repeat 8   (degrees [4 3 4] :major :F3))
                                (repeat 16  (degrees [1 3 1] :major :F3))
                                (repeat 8   (degrees [4 3 4] :major :F3))
                                (repeat 16  (degrees [1 3 1] :major :F3))))))

(pattern! note1-dur-b
          [1/2 1/2 1/12 1/12]
          [1/8 1/8 1/8 1/8])

(pattern! note-dur-b [1. 1. 1.5])

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
)

(comment
  (fadeout puck)
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

(pattern! effects2-seq-buf [1 1 1 1 0 1])

(pattern! effects2-seq-buf [1 1 1  1 0 0  0 1 0  0 0 0  0 0 0])

(pattern! effects2-seq-buf [1 0 0  1 1 1  0 0 0  1 0 0])


(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.04 :num-steps 16 :buf bell-s) (range 0 16))))
(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 16 :buf (buffer-mix-to-mono deep-bass-kick-s)) (range 0 3))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.04 :num-steps 8 :buf vogel-clap-s) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.2 :num-steps 8 :buf virus-kick-s) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.2 :num-steps 8 :buf pulse-s) (range 0 16))))


(def clap2-drums (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.02 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))
(def clap-drums  (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.01 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16))))

(kill drum-effects-g)

(do
  (definst wobbling
    [amp 0.8 t 4
     mix-rate 0.8 room-rate 0.5
     beat-bus (:count time/beat-2th) beat-trg-bus (:beat time/beat-2th)
     amt 0.3
     notes-buf 0 dur-buf 0
     max-delay 0.4
     delay 0.3
     decay 0.5]
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
          echo       (comb-n reverb max-delay delay decay)]
      (* amp echo)))

  (defonce note1-b (buffer 256))
  (defonce note2-b (buffer 256))
  (defonce note3-b (buffer 256))

  (defonce grumblers-g (group "the grumblers"))

  (def grumble1 (wobbling [:head grumblers-g] :notes-buf note1-b :amp 0.4 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
))


  (def grumble2 (wobbling [:head grumblers-g] :notes-buf note2-b :amp 0.4 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
))


  (def grumble3 (wobbling [:head grumblers-g]  :notes-buf note3-b :amp 0.4 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
))
 )


(defn grumble-chords []
  (pattern! note1-dur-b [3])
  (let [_ [0 0 0]
        [F21 F22 F23 F24 F25 F26 F27] (map #(chord-degree %1 :F2 :major 3) [:i :ii :iii :iv :v :vi :vii])
        [F31 F32 F33 F34 F35 F36 F37] (map #(chord-degree %1 :F3 :major 3) [:i :ii :iii :iv :v :vi :vii])
        [F41 F42 F43 F44 F45 F46 F47] (map #(chord-degree %1 :F3 :major 3) [:i :ii :iii :iv :v :vi :vii])

        [C41 C42 C43 C44 C45 C46 C47] (map #(chord-degree %1 :C4 :major 3) [:i :ii :iii :iv :v :vi :vii])
        [C31 C32 C33 C34 C35 C36 C37] (map #(chord-degree %1 :C3 :major 3) [:i :ii :iii :iv :v :vi :vii])
        ]
    (let [gap (choose [_ F33])
          octave-jump (choose [false true])
          c-mood (choose [false true])
          chord-pat
          (concat (repeat 8 [F33 F33 gap])
                  (repeat 8 [F34 gap F31])
                  (repeat 8 [F33 F31 F31])
                  (repeat 8 [F34 F31 F31])

                  (if c-mood
                    (concat
                     (repeat 8 [F31 F32 F32])
                     (repeat 8 [F34 gap F34])
                     (repeat 8 [F31 F31 F31])
                     (repeat 8 [F34 F34 F34]))

                    (concat
                     (repeat 8 [F33 F31 gap])
                     (repeat 8 [F35 F31 F31])
                     (repeat 8 [F33 F31 F31])
                     (repeat 8 [F36 F31 F31])))

                  (if c-mood
                    (concat
                     (repeat 8 [C44 C41 C41])
                     (repeat 8 [C44 C41 C41])
                     (repeat 8 [C41 C41 C41])
                     (repeat 8 [C44 C41 C41]))
                    (concat
                     (if octave-jump
                       (concat (repeat 8 [F44 F41 F41])
                               (repeat 8 [F44 F41 F41]))
                       (concat (repeat 8 [F33 F31 gap])
                               (repeat 8 [F35 F31 F31])))
                     (repeat 8 [F31 F31 F31])
                     (repeat 8 [F34 F31 F31])))

                  (if octave-jump
                    (concat
                     (repeat 8 [F43 F41 F41])
                     (repeat 8 [F46 F41 F41]))
                    (concat
                     (repeat 8 [F33 F31 F31])
                     (repeat 8 [F36 F31 F31]))))

          ;;        chord-pat (map (fn [notes] (map #(var-get (resolve (symbol (name %1)))) notes)) chord-pat)
          ]
      (let [chord-bufs (shuffle [note1-b note2-b note3-b])] ;; Play around with some random inversions
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))))

(on-beat-trigger 512 #(grumble-chords))
(remove-all-beat-triggers)

(ctl grumblers-g :beat-bus (:count time/beat-12th) :beat-trg-bus (:beat time/beat-12th))
(ctl grumblers-g :attack 1.0 :amt 4 :t 0.1)

(def grumble (wobbling :notes-buf note-b :amp 0.4 :dur-buf note1-dur-b
                       :beat-bus (:count time/beat-1th)
                       :beat-trg-bus (:beat time/beat-1th) :attack 0.9))

(ctl grumble :room-rate 1 :mix-rate 0.8 :amt 0.3 :attack 0.002 :decay 0.001 :amp 0.0)
(ctl grumble :amt 0.6 :attack 0.1 :decay 0.8 :mix-rate 0.5 :amp 0.0)
(fadein grumble 0.25 0.01)
(kill wobbling)
(do
  (defonce tonal-notes-b (buffer 256))
  (defonce tonal-reverb-b (buffer 256))
  (defonce tonal-dur-b (buffer 256))
  (defonce tonal-amp-b (buffer 256)))

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

  (defonce tonal-notes2-b (buffer 256))
  (defonce tonal-reverb2-b (buffer 256))
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

  (def tone (tonal :amp 0.8 :notes-buf tonal-notes-b :reverb-buf tonal-reverb-b :rev-time 0 :room-rate 0 :dur-buf tonal-dur-b))
  )


(do
  (definst sharp-twang [notes-buf 0 amp 1
                        beat-trg-bus (:beat time/beat-2th)
                        beat-bus (:count time/beat-2th)
                        dur-buf 0
                        attack-buf 0
                        release-buf 0
                        amp-buf 0
                        overtones 1.5
                        decay 3
                        sustain 1
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
          o1 (free-verb o1 :mix (+ 0.5 (* 0.5 (sin-osc:kr 0.5))) :room 5)
          o2 (free-verb o2 :mix (+ 0.5 (* 0.2 (sin-osc:kr 1.0))) :room 5)
          e (env-gen (adsr :attack attack :release release :decay decay :sustain 0.1) :time-scale dur :gate gate-trig)
          ]
      (* e (* amp-rate (+ 3 amp)) [o1 o2]))
    )

  (kill sharp-twang)

  (defonce twang-notes-buf (buffer 256))
  (defonce twang-dur-buf (buffer 256))
  (defonce twang-attack-buf (buffer 256))
  (defonce twang-release-buf (buffer 256))
  (defonce twang-amp-buf (buffer 256))

  ;;(ctl sharp-t :overtones 1.5 :amp 4)
  ;;(ctl sharp-t :overtones 0.01 :amp 3 :decay 0.01 :sustain 0.01)

  (def sharp-t (sharp-twang :notes-buf twang-notes-buf :amp 0 :dur-buf twang-dur-buf
                            :attack-buf twang-attack-buf :release-buf twang-release-buf
                            :amp-buf twang-amp-buf))
  (ctl sharp-t :amp 4.0)
  (fadein sharp-t 4.0)

  (pattern! twang-release-buf [0.8  0.8   0.8   4   4   4   4   4   4   4   4   4   4])
  (pattern! twang-attack-buf  [0.08 0.08 0.08 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3])
  (pattern! twang-dur-buf [2 2 2 1/2 1/2 1/2 1 1 1 1 1 1/2 1/2])
  (pattern! twang-amp-buf
            (repeat 3 [1 1 1 1/2 1/2 1/2 1 1 1 1 1 1/2])
            [1.2 1.2 1.2 1/2 1/2 1/2 1 1 1 1 1 1/2])

  (pattern! twang-notes-buf
            (degrees [1 5 3 0 3 0 3 2] :major :F3) [0 0 0 0]
            (degrees [3 5 4 0 4 0 4 3] :major :F3) [0 0 0 0]
            (degrees [5 7 5 0 5 0 5 3] :major :F4) [0 0 0 0]
            (degrees [5 7 6 0 5 0 5 3] :major :F4) [0 0 0 0]
            )

  (chord-degree :i (note-at-octave :F 3) :major 3)
    ;;  72 76 79 82
;;  (stop)
  (degrees [3] :major :F3)


  (defonce twang2-release-buf (buffer 256))
  (defonce twang2-attack-buf (buffer 256))
  (defonce twang3-note-buf (buffer 256))

  (pattern! twang2-release-buf [2.0])
  (pattern! twang2-attack-buf  [0.8])

  (def background-sharp-t (sharp-twang :notes-buf twang3-note-buf :amp 2 :dur-buf twang-dur-buf
                                       :attack-buf twang2-attack-buf :release-buf twang2-release-buf
                                       :amp-buf twang-amp-buf))

  (fadein background-sharp-t 2 0.1)
;;  (kill sharp-twang)

;;(ctl background-sharp-t :overtones 0.5 :amp 4 :attack 1.0 :release 2.0 :decay 2.0 :sustain 2.0)
(pattern! twang3-note-buf
          (repeat 8 (degrees [3] :major :F3)) [0 0]
          (repeat 8 (degrees [6] :major :F3)) [0 0]
          (repeat 8 (degrees [3] :major :F3)) [0 0]
          (repeat 8 (degrees [6] :major :F3)) [0 0]

          (repeat 8 (degrees [5] :major :F3)) [0 0]
          (repeat 8 (degrees [6] :major :F3)) [0 0]
          (repeat 8 (degrees [5] :major :F3)) [0 0]
          (repeat 8 (degrees [8] :major :F3)) [0 0]

          (repeat 8 (degrees [3] :major :F3)) [0 0]
          (repeat 8 (degrees [6] :major :F3)) [0 0]
          (repeat 8 (degrees [3] :major :F3)) [0 0]
          (repeat 8 (degrees [6] :major :F3)) [0 0]

          (repeat 8 (degrees [5] :major :F3)) [0 0]
          (repeat 8 (degrees [7] :major :F3)) [0 0]
          (repeat 8 (degrees [5] :major :F3)) [0 0]
          (repeat 8 (degrees [8] :major :F3)) [0 0]
          ;;(repeat 8 (degrees [1 3 1] :major :F3))
          )

(pattern! twang-notes-buf
          (degrees [3 1 1 3 1 1   1 0 0 1 0 0] :major :F3)
          (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3)
          (degrees [1 0 0 1 0 0   1 0 0 1 0 0] :major :A3)
          (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3))

  (pattern! twang-notes-buf
             (degrees [3 1 1 3 1 1   1 0 0 1 0 0] :major :F4)
             (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3)
             (degrees [1 0 0 1 0 0   1 0 0 1 0 0] :major :A3)
             (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3)

             (degrees [3 1 1 3 1 1   4 1 1 4 1 1] :major :F3)
             (degrees [5 3 3 5 3 3   5 3 3 5 3 3] :major :F3)
             (degrees [3 1 1 3 1 1   3 1 1 3 1 1] :major :F4)
             (degrees [6 3 3 6 3 3   6 5 5 6 5 5] :major :F3)

             (degrees [1 3 3 1 3 3   1 5 5 1 5 5] :major :C4)
             (degrees [4 1 1 4 1 1   4 1 1 4 1 1] :major :C4)
             (degrees [1 3 3 1 3 3   1 5 5 1 5 5] :major :C4)
             (degrees [4 3 3 4 3 3   4 5 5 4 5 5] :major :C4)

             (degrees [3 0 0 3 0 0   3 1 1 3 1 1] :major :F4)
             (degrees [5 0 0 5 0 0   5 0 0 5 0 0] :major :F4)
             (degrees [3 0 0 3 0 0   3 0 0 3 0 0] :major :F4)
             (degrees [6 0 0 6 0 0   6 0 0 6 0 0] :major :F4)
            ;;---------------------------------------------

              (degrees [3 1 1 3 1 1   1 0 0 1 0 0] :major :F4)
              (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3)
              (degrees [1 0 0 1 0 0   1 0 0 1 0 0] :major :F3)
              (degrees [4 0 0 4 0 0   4 0 0 4 0 0] :major :F3)

              (degrees [3 1 1 3 1 1   4 1 1 4 1 1] :major :F3)
              (degrees [5 3 3 5 3 3   5 3 3 5 3 3] :major :F3)
              (degrees [3 1 1 3 1 1   3 1 1 3 1 1] :major :F4)
              (degrees [6 3 3 6 3 3   6 5 5 6 5 5] :major :F3)

              (degrees [1 3 3 1 3 3   1 5 5 1 5 5] :major :C4)
              (degrees [4 1 1 4 1 1   4 1 1 4 1 1] :major :C4)
              (degrees [1 3 3 1 3 3   1 5 5 1 5 5] :major :C4)
              (degrees [4 3 3 4 3 3   4 5 5 4 5 5] :major :C4)

              (degrees [3 0 0 3 0 0   3 1 1 3 1 1] :major :F4)
              (degrees [5 0 0 5 0 0   5 0 0 5 0 0] :major :F4)
              (degrees [3 0 0 3 0 0   3 0 0 3 0 0] :major :F4)
              (degrees [7 5 5 7 5 5   7 5 5 7 5 5] :major :F4)
              )


  (let [_ [0 0 0]
        [fm31 fm32 fm33 fm34 fm35 fm36 fm37] (map #(chord-degree %1 :F3 :minor 3) [:i :ii :iii :iv :v :vi :vii])
        [f21 f22 f23 f24 f25 f26 f27] (map #(chord-degree %1 :F2 :major 3) [:i :ii :iii :iv :v :vi :vii])
        [f31 f32 f33 f34 f35 f36 f37] (map #(chord-degree %1 :F3 :major 3) [:i :ii :iii :iv :v :vi :vii])
        [f41 f42 f43 f44 f45 f46 f47] (map #(chord-degree %1 :F4 :major 3) [:i :ii :iii :iv :v :vi :vii])]
    (let [chord-pat
          [f31]]
      (let [chord-bufs (shuffle [n1 n2 n3])] ;; Play around with some random inversions
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))

  (kill sharp-twang)
  (remove-all-beat-triggers)
  (stop)

  (pattern! twang-dur-buf [1
                           1/2 1   3  3 3 3     1/2 1/4 5   5 5 5
                           1/2 2 1    3 3 3      1/2 1/4 5   5 5 5
                           1/4 1/2 1  1 1/4 1/2  1/2 1/4 1   1 1 1
                           ]);;

  (pattern! twang-dur-buf [1])

  (pattern! twang-release-buf [8])

  (pattern! twang-release-buf [4])
  (pattern! twang-attack-buf  [2.0])
  (pattern! twang-amp-buf     [1.0])

  (do
    (sharp-twang :notes-buf n1 :amp 4 :dur-buf twang-dur-buf :attack-buf twang-attack-buf :release-buf twang-release-buf :amp-buf twang-amp-buf :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th))
    (sharp-twang :notes-buf n2 :amp 4 :dur-buf twang-dur-buf :attack-buf twang-attack-buf :release-buf twang-release-buf :amp-buf twang-amp-buf :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th))
    (sharp-twang :notes-buf n3 :amp 4 :dur-buf twang-dur-buf :attack-buf twang-attack-buf :release-buf twang-release-buf :amp-buf twang-amp-buf :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th)))


  (pattern! twang-notes-buf (degrees [5 7 5 7] :major :F4) [0 0 0 0 0 0 0 0])
  (pattern! twang-notes-buf (degrees [7] :major :F4) [0 0 0 0])

  ;;(ctl sharp-t :notes-buf note-b)

  (defonce twang-notes2-buf (buffer 256))
  (defonce twang-dur2-buf (buffer 256))
  (defonce twang-attack2-buf (buffer 256))
  (defonce twang-release2-buf (buffer 256))
  (defonce twang-amp2-buf (buffer 256))

  (kill sharp-twang)
;;  (stop)

  (def light-twang (sharp-twang :notes-buf twang-notes2-buf :amp 0 :dur-buf twang-dur2-buf
                                 :attack-buf twang-attack2-buf :release-buf twang-release2-buf
                                 :amp-buf twang-amp2-buf :beat-trg-bus (:beat time/beat-2th) :beat-bus  (:count time/beat-2th)))

  (fadein light-twang 5 0.1)
  (ctl light-twang :amp 20)

  (pattern! twang-release2-buf [4])
  (pattern! twang-attack2-buf  [0.01])
  (pattern! twang-dur2-buf [3])
  (pattern! twang-amp2-buf (repeat 3 [0.2]))

  (pattern! twang-notes2-buf
            (repeat 1 (concat (repeat 1 (degrees [1] :major :F3)) [0 0 0 0 0 0 0]))
            ;;(repeat 2 [0 0 0 0 0 0 0 0])
            (repeat 1 (concat (repeat 1 (degrees [3] :major :F3)) [0 0 0 0 0 0 0]))
            (repeat 1 (concat (repeat 1 (degrees [5] :major :F3)) [0 0 0 0 0 0 0]))
            (repeat 1 (concat (repeat 1 (degrees [3] :major :F3)) [0 0 0 0 0 0 0]))

            )

  (pattern! twang-notes2-buf
            [0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 (degrees [7] :major :F3)]
            [0 0 0 0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0 0 0 0]


    ;;        [0 0 0 0] (degrees [5 7 5 0 0 0 0 0] :major :F3)
  ;;          [0 0 0 0] (degrees [7 8 7 0 0 0 0 0] :major :F3)
;;            [0 0 0 0] (degrees [7 8 7 0 0 0 0 0] :major :F3)
            )


  (pattern! twang-notes2-buf
            (repeat 8 (degrees [0 0 0] :major :F3))
            (repeat 8  [0 0 0])
            (repeat 8 (degrees [0 0 0] :major :F3))
            (repeat 8  [0 0 0])

            (repeat 8 (degrees [3 0 0] :major :F3))
            (repeat 8  [0 0 0])
            (repeat 8 (degrees [3 0 0] :major :F3))
            (repeat 8  [0 0 0])


            (repeat 8  [0 0 0])
            (repeat 8  [0 0 0])
            (repeat 8  [0 0 0])
            (repeat 8  [0 0 0])


            (repeat 8 (degrees [3 0 0] :major :F4))
            (repeat 8  [0 0 0])
            (repeat 8 (degrees [3 0 0] :major :F4))
            (repeat 8  [0 0 0])
)


  (pattern! twang-notes2-buf
            (repeat 4 [0 0 0])
            (repeat 4 (degrees [7 0 0] :major :F3))
            (repeat 4 [0 0 0])
            (repeat 4 (degrees [4 0 0] :major :F3))

            (repeat 4 [0 0 0])
            (repeat 4 (degrees [0 0 0] :major :F3))
            (repeat 4 [0 0 0])
            (repeat 4 (degrees [6 0 0] :major :F3))

            (repeat 4 [0 0 0])
            (repeat 4 (degrees [4 0 0] :major :F3))
            (repeat 4 [0 0 0])
            (repeat 4 (degrees [4 0 0] :major :F3))

            (repeat 4 [0 0 0])
            (repeat 4 (degrees [5 0 0] :major :F3))
            (repeat 4 [0 0 0])
            (repeat 4 (degrees [6 0 0] :major :F3)))


  (pattern! twang-release-buf [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.1 0.9 2.0 0.5 0.5 ])
  (pattern! twang-attack-buf  [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.1 0.5 0.3 0.5 0.5 ])
  (pattern! twang-dur-buf     [1 1 1 1 1 1 1 1 1/2 2   1 1 ])
  (pattern! twang-amp-buf     [1 1 1 1 1 1 1 1 0.8 0.9 1 1 ])


  (pattern! twang-amp-buf      [1])
  (pattern! twang-attack-buf   [0.3])
  (pattern! twang-release-buf  [2])
  (pattern! twang-dur-buf      [0.8])

  (pattern! twang-notes-buf
            [0 0 0 0 0 0 0 0] (degrees [5] :major :F3)  (degrees [3] :major :F4) (degrees [3] :major :F4) [0]
            [0 0 0 0 0 0 0 0] (degrees [3] :major :F3)  (degrees [1] :major :F4) (degrees [1] :major :F4) [0]
            [0 0 0 0 0 0 0 0] (degrees [7] :major :F3)  (degrees [5] :major :F4) (degrees [5] :major :F4) [0]
            [0 0 0 0 0 0 0 0] (degrees [8] :major :F3)  (degrees [7] :major :F4) (degrees [7] :major :F4) [0])

  (comment
    (pattern! twang-notes-buf
              (degrees [0 0 0 0 0 0 0 3 4 6 4 0 3 2 0 0 0 0 1 6 0 1 0 0 0 0 0 0 4 0 0 3 1 1 2 0 0 3 3 0 0 3 4 0 1 3 0 0 0 0 1 0 1 7 0 0 5 6 3 0 4 0 9 0] :major :F4)))
  )

;;(ctl (foundation-output-group) :master-volume 2)

(do
  (definst sawer [freq 300
                  notes-buf 0 amp 1
                  beat-trg-bus (:beat time/beat-1th)
                  beat-bus (:count time/beat-1th)
                  dur-buf 0
                  attack 0.4
                  decay 0.2
                  release 8
                  sustain 8
                  rev-time 4]

    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          freq (midicps note)
          gate-trig (and (> note 0) trg)

          src (bpf (lf-tri freq) 2000)
          src (comb-l src 2 2 10)
          src (g-verb src :roomsize 200 :revtime rev-time :damping 0.5)
          e (env-gen (adsr :attack attack :release release :sustain sustain :decay decay) :gate gate-trig :time-scale dur)
          ]
      (pan2 (* e amp src))))

  (kill sawer)

  (defonce sawer-notes-buf (buffer 256))
  (defonce sawer-dur-buf (buffer 256))

  (def undertone (sawer :notes-buf sawer-notes-buf :amp 1.0 :dur-buf sawer-dur-buf :rev-time 140
                        :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th)
                        :attack 0.2 :release 2 :sustain 2 :decay 2))

  (ctl undertone :amp 0.4)
  (pattern! sawer-notes-buf
            [0 0 0 0 0] (degrees [3] :major :F4) (degrees [1] :major :E4) (degrees [5] :major :F4) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F4) (degrees [5] :major :C4) (degrees [7] :major :F4) [0 0 0 0]
            [0 0 0 0 0] (degrees [3] :major :F4) (degrees [1] :major :E4) (degrees [6] :major :F4) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F4) (degrees [3] :major :F3) (degrees [7] :major :F4) [0 0 0 0]

            [0 0 0 0 0] (degrees [3] :major :F3) (degrees [1] :major :A3) (degrees [5] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F3) (degrees [5] :major :A3) (degrees [7] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [3] :major :F3) (degrees [1] :major :F3) (degrees [6] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F3) (degrees [3] :major :F3) (degrees [7] :major :F3) [0 0 0 0]

            [0 0 0 0 0] (degrees [3] :major :F3) (degrees [1] :major :F3) (degrees [5] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F3) (degrees [5] :major :F3) (degrees [7] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [3] :major :F3) (degrees [1] :major :F3) (degrees [6] :major :F3) [0 0 0 0]
            [0 0 0 0 0] (degrees [1] :major :F3) (degrees [3] :major :F3) (degrees [8] :major :F3) [0 0 0 0]
            )

;;  (kill sawer)
  (defonce sawer-notes2-buf (buffer 256))
  (def undertone2 (sawer :notes-buf sawer-notes2-buf :amp 1.2 :dur-buf sawer-dur-buf  :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th)))
  (ctl undertone2 :attack 0.3 :release 3.0 :sustain 3.0 :decay 0.1 :amp 0.1)


  (pattern! sawer-notes2-buf
            (degrees [1 1 1 1 1] :major :F4) [0 0 0] (degrees [1 1 1 1] :major :F4)
            (degrees [1 1 1 1 1] :major :F4) [0 0 0] (degrees [1 1 1 1] :major :F4)
            (degrees [1 1 1 1 1] :major :F4) [0 0 0] (degrees [1 1 1 1] :major :F4)
            (degrees [1 1 1 1 1] :major :F4) [0 0 0] (degrees [1 1 1 1] :major :F4)

            (degrees [1 1 1 1 1] :major :F3) [0 0 0] (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0] (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0] (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0] (degrees [1 1 1 1] :major :F3)

            (degrees [1 1 1 1 1] :major :F3) [0 0 0]  (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0]  (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0]  (degrees [1 1 1 1] :major :F3)
            (degrees [1 1 1 1 1] :major :F3) [0 0 0]  (degrees [1 1 1 1] :major :F3))

  (pattern! sawer-notes2-buf
            (degrees [3 1 2] :major :F3)  (repeat 9 [0])
            (degrees [5 3 2] :major :F3)  (repeat 9 [0])
            (degrees [3 1 2] :major :F4)  (repeat 9 [0])
            (degrees [5 3 7] :major :F4)  (repeat 9 [0])
            )

  (pattern! sawer-notes-buf
            (degrees [0 0 0 0 0 3 1 5 0 0 0 0] :major :F3)
            (degrees [0 0 0 0 0 1 5 7 0 0 0 0] :major :F3)

            (degrees [0 0 0 0 0 3 1 6 0 0 0 0] :major :F3)
            (degrees [0 0 0 0 0 1 3 7 0 0 0 0] :major :F3)
            )

  (pattern! sawer-dur-buf [0.9 0.5 0.7 0.8 0.9 1 3 8 0.9 0.8 0.5 0.5])
  (pattern! sawer-dur-buf [1.0])

  ;;(pattern! sawer-dur-buf [0.3 0.3 0.3 0.3 0.3 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1 1.5 2 3 4 5])

  (pattern! sawer-notes-buf
            [0 0 0 0 0 0 0] (degrees [3] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [1] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [5] :major :F4) [0 0 0 0]
            [0 0 0 0 0 0 0] (degrees [7] :major :F4) [0 0 0 0])
)

;;(ctl undertone :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :amp 0.2 :attack 0.9 :rev-time 10)

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
  (slobber :amp 0.19)
)

(do
  (defonce twang-but (buffer 256))
  (definst echoey-twang [amp 1
                         beat-trg-bus (:beat time/beat-16th)
                         beat-bus      (:count time/beat-16th)
                         notes-buf 0]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          freq (midicps note)
          gate-trg (and (> note 0) trg)

          e (env-gen (adsr) :gate gate-trg)
          a [1 2 4 5]
          src (lag (blip:ar a) (* 1 (+ 1 (lf-saw:ar (/ 1 (+ 2.25 a)) (/ 2 a)))))
          src (splay:ar (* (sin-osc:ar freq (* 99 (blip:ar 2 (sin-osc 99)))) src))
          src (g-verb:ar src 99 6 0.7)]
      (* e amp src)))

  (echoey-twang :amp 1 :freq-buf twang-but)
;;  (pattern! echo-note-b (degrees [8] :major :F0))
)

(comment
  (fx/fx-distortion-tubescreamer)

  (fadeout puck)
  (fadeout tone)
  (fadeout grumble)
  (fadeout undertone)

  (fadeout-master)

  (fx/fx-chorus)
  (fx/fx-reverb)

  (recording-start "~/Desktop/pop9.wav")
  (recording-stop)
  (stop)
  (remove-all-beat-triggers)
  )

;;(fadeout-master)
;;(echoey-buf one-moment-please-s :amp 0.1)
;;(echoey-buf beep-s :amp 0.01)
;;(echoey-buf goodbye-s :amp 0.2)
;;(spacy beep-s)

;;(on-beat-trigger 192 #(do (echoey-buf one-moment-please-s :amp 0.02)))
;;(on-beat-trigger 96 #(do (echoey-buf afraid-s :amp 0.025)))
;;(on-beat-trigger 60  #(do (echoey-buf (dirt :wind 0) :amp 0.15)))


;;(on-beat-trigger 128 #(do (echoey-buf (dirt :arp 0) :amp 0.09)))
;;(on-beat-trigger 32  #(do (echoey-buf (dirt :moog 7) :amp 0.09)))
;;(on-beat-trigger 128 #(do (echoey-buf (dirt :crows 1) :amp 0.19)))

(remove-all-beat-triggers)
(kill drum-effects-g)
(stop)
