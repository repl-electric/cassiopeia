(ns cassiopeia.destination.mr
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  (:use cassiopeia.dirt)
  (:require [cassiopeia.engine.timing :as time]
            [clojure.math.numeric-tower :as math]
            [overtone.studio.fx :as fx]))

;;(ctl time/root-s :rate 8.)

(do
  (defonce note1-dur-b (buffer 256))

  (definst wobbling
    [amp 0.8 t 0.01
     mix-rate 0.0
     room-rate 0.0
     beat-bus (:count time/beat-2th) beat-trg-bus (:beat time/beat-2th)
     amt 0.3
     notes-buf 0 dur-buf 0
     max-delay 0.01
     delay 0.01
     decay 0.01
     lag-time 0]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          dur (buf-rd:kr 1 dur-buf cnt)
          freq (midicps note)
          gate-trg (and (> note 0) trg)

          f-env      (env-gen (perc t t) gate-trg 1 0 dur)
          src        (saw [freq (* freq 1.01)])
          signal     (rlpf (* 0.3 src)
                           (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
          k          (/ (* 2 amt) (- 1 amt))
          distort    (/ (* (+ 2 k) signal) (+ 2 (* k (abs signal))))
;;          gate       (pulse (* 2 (+ 1 )))
          ;;compressor (compander distort (pulse gate-trg) 0.01 1 0.5 0.01 0.01)
          dampener   (+ 1 (* 0.5))
          reverb     (free-verb distort mix-rate room-rate dampener)
          echo       (comb-n reverb max-delay delay decay)
;;          echo       (lag echo lag-time)
;;          echo (free-verb echo :mix 0.1)
          ]
      (* amp echo)))

  (defonce note1-b (buffer 256))
  (defonce note2-b (buffer 256))
  (defonce note3-b (buffer 256))

  (kill wobbling)
  (defonce grumblers-g (group "the grumblers"))

  (def grumble1 (wobbling [:head grumblers-g] :notes-buf note1-b :amp 1.0 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
                          :lag-time 0.0
                          :t 0.5))


  (def grumble2 (wobbling [:head grumblers-g] :notes-buf note2-b :amp 1.0 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
                          :lag-time 0.0
                          :t 0.5))

  (def grumble3 (wobbling [:head grumblers-g]  :notes-buf note3-b :amp 1.0 :dur-buf note1-dur-b
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th)
                          :lag-time 0.0
                          :t 0.5)))

(on-beat-trigger 96 #(do
                         (ctl grumblers-g :t 0.01)))

(remove-on-beat-trigger)

(defn grumble-chords []
  (let [_ [0 0 0]
        [F21 F22 F23 F24 F25 F26 F27] (map #(chord-degree %1 :F2 :minor	 3) [:i :ii :iii :iv :v :vi :vii])
        [F31 F32 F33 F34 F35 F36 F37] (map #(chord-degree %1 :F3 :major	 3) [:i :ii :iii :iv :v :vi :vii])
        [F41 F42 F43 F44 F45 F46 F47] (map #(chord-degree %1 :F4 :minor 3) [:i :ii :iii :iv :v :vi :vii])

        ;;F3II
        ;;ii
        [Fa31 Fa32 Fa33 Fa34 Fa35 Fa36 Fa37] (map #(chord-degree %1 :F3 :minor 3) [:i :ii :iii :iv :v :vi :vii])
        [Fa41 Fa42 Fa43 Fa44 Fa45 Fa46 Fa47] (map #(chord-degree %1 :F3 :major 3) [:i :ii :iii :iv :v :vi :vii])

        [C41 C42 C43 C44 C45 C46 C47] (map #(chord-degree %1 :C4 :minor	3) [:i :ii :iii :iv :v :vi :vii])
        [C31 C32 C33 C34 C35 C36 C37] (map #(chord-degree %1 :C3 :minor	3) [:i :ii :iii :iv :v :vi :vii])]

    (pattern! note1-dur-b [1/12 1/12 1/12 1/12 1/2 2 2 2])
    (let [chord-pat
          (concat [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]

                  [Fa31 _ Fa31 Fa31 Fa33 (degrees [3] :minor :F4) _ _]
                  [Fa31 _ Fa31 Fa31 Fa33 (degrees [3] :minor :F4) _ _]
                  [Fa31 _ Fa31 Fa31 Fa33 (degrees [3] :minor :F4) _ _]
                  [Fa31 _ Fa31 Fa31 Fa33 (degrees [3] :minor :F4) _ _]

                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]
                  [F31 _ F31 F31 F33 (degrees [1] :major :F4)  _ _]

                  [F31 F32 F41 F31 F43 (degrees [3] :minor :F4) F31 F31]
                  [F31 F32 F41 F31 F43 (degrees [3] :minor :F4) F31 F31]
                  [F31 F32 F41 F31 F43 (degrees [3] :minor :F4) F31 F31]
                  [F31 F32 F41 F31 F43 (degrees [3] :minor :F4) F31 F31]
                  )]

      (let [chord-bufs (shuffle [note1-b note2-b note3-b])] ;; Play around with some random inversions
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))
  )

(kill wobbling)

(defonce note4-b (buffer 256))
(def grumble4 (wobbling [:head grumblers-g]  :notes-buf note4-b :amp 4.0 :dur-buf note1-dur-b
                        :beat-bus (:count time/beat-1th)
                        :beat-trg-bus (:beat time/beat-1th)))

(do
  (defonce w-note-b (buffer 256))
  (definst deep-bass [notes-buf w-note-b
                      beat-trg-bus (:beat time/beat-4th)
                      beat-bus     (:count time/beat-4th)
                      noise-level 0.05
                      amp 1]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          gate-trg (and (> note 0) trg)
          freq (midicps note)
          noize (* noise-level (pink-noise))
          src (mix [(lpf (saw freq) 300)
                    (lpf noize 1000)])
          src (g-verb src 300 2 0.2)
          e (env-gen (perc 0.4 0.9) :gate gate-trg)
          amp (+ (* amp 2) amp)
          ]
      (* amp e src)))
  (kill deep-basz)
  (deep-basz :amp 0.7 :noise-level 0.05
             :notes-buf w-note-b
             :beat-trg-bus (:beat time/beat-4th)
             :beat-bus (:count time/beat-4th))

  (deep-basz :amp 0.7 :noise-level 0.05
             :notes-buf w-note2-b
             :beat-trg-bus (:beat time/beat-2th)
             :beat-bus (:count time/beat-2th))

  (pattern! w-note2-b
            (repeat 16  0)
            (repeat 1  [0 0 0 0 (degrees [3] :minor :F2) 0 (degrees [3] :minor :F2)
0])
            (repeat 1  [ 0 0 0 0
                        (degrees [4] :minor :F2) 0 (degrees [4] :minor :F2) 0 ]))

  (pattern! w-note-b
            (repeat 8  (degrees [1] :major :F2))
            (repeat 4  (degrees [3] :minor :F2))
            (repeat 4  (degrees [4] :minor :F2)))

  (pattern! w-note-b
            (repeat 4  [(degrees [1] :major :F2) 0])
            (repeat 4  [(degrees [1] :major :F2) (degrees [1] :major :F2)])
            )

  (pattern! w-note-b
            (repeat 8  [(degrees [1] :major :F2) ])
            (repeat 1  [(degrees [3] :major :F2) (degrees [3] :major :F2) 0 0])
            (repeat 1  [(degrees [4] :major :F2) (degrees [4] :major :F2) 0 0]))


  )

(stop)

(do
  (defonce s-note-b (buffer 256))
  (definst sawy [notes-buf s-note-b
                 beat-trg-bus (:beat time/beat-1th)
                 beat-bus     (:count time/beat-1th)
                 noise-level 0
                 amp 1]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          gate-trg (and (> note 0) trg)
          freq (midicps note)
          noize (* noise-level (pink-noise))
          src (lpf (mix [noize (pulse:ar note 0.01)]) 2000)
          src (pitch-shift src 0.01 0.9 1 0.1)
          e (env-gen (adsr :release 0.6 :sustain 0.6) :gate gate-trg)]
      (* (+ (* amp 1) amp) e src)))

  (kill sawy)
  (sawy :noise-level 0.1 :amp 0.3)
  (pattern! s-note-b
            (repeat (* 2 4) [(degrees [3] :minor :F3)]) (repeat (* 2 4) [0])
            (repeat (* 2 4) [(degrees [5] :minor :F4)]) (repeat (* 2 4) [0])))

(do
  (defonce f-note-b (buffer 256))
  (definst fizzal [notes-buf s-note-b
                 beat-trg-bus (:beat time/beat-1th)
                 beat-bus     (:count time/beat-1th)
                 noise-level 0.05
                 amp 1]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 notes-buf cnt)
          gate-trg (and (> note 0) trg)
          freq (midicps note)
          noize (bpf (* noise-level (pink-noise)) 1000)
          src (sum [(pulse:ar freq 5.0) noize (sin-osc (* 0.25 freq))])
          ;; src (pitch-shift src 0.01 0.9 1 0.1)
          e (env-gen (adsr :release 0.9 :sustain 0.9) :gate gate-trg)]
      (* (+ (* amp 1) amp) e src)))

  (kill fizzal)
  (fizzal :amp 0.3)
  (pattern! f-note-b
            (repeat (* 2 4) [(degrees [1] :minor :F3)]) (repeat (* 2 4) [0])
            (repeat (* 2 4) [(degrees [3] :minor :F3)]) (repeat (* 2 4) [0])))

(stop)
(on-beat-trigger 64 #(grumble-chords))
(remove-all-beat-triggers)


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

(pattern! effects2-seq-buf [1 1 0 0 0 0 0 0])

(pattern! effects2-seq-buf [1 1 1  1 0 0  0 1 0  0 0 0  0 0 0])

(pattern! effects2-seq-buf [1 0 0  1 1 1  0 0 0  1 0 0])

(def clap2-drums (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))
(def clap-drums  (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.1 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16))))

(kill drum-effects-g)
(kill drums-g)

(pattern! bass-notes-buf (repeat 16 [:F2]) (repeat 16 [:G2]))
(pattern! kick-seq-buf (repeat 14 [1 0 0 0]) [0 0 0 0] [0 1 1 1])

(def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 1)))
(ctl drums-g :mod-freq 10.2 :mod-index 0.1 :noise 0)

(pattern! hats-buf  (repeat 14 [0 0 1 0]) [0 0 1 0] [0 0 0 0])

(def hats (doall (map #(high-hats [:head drums-g] :amp 0.2 :mix (nth (take 32 (cycle [1.0 1.0])) %1) :room 4 :note-buf bass-notes-buf :seq-buf hats-buf :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 0.9 :mix 0.0 :room 1 :amp 0.3)

;;(stop)
