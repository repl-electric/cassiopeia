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

(ctl time/root-s :rate 8.)

(defonce w-note6-b (buffer 256))

(defonce note1-dur-b (buffer 256))
(definst wobbling
  [amp 0.8
   t 0.01
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

(definst deep-basz-pi [amp 1
                    notes-buf 0
                    noise-level 0.05
                    beat-trg-bus (:beat time/beat-4th)
                       beat-bus     (:count time/beat-4th)
                       attack-buf 0
                       release-buf 0
                       amp-buf 0
                    attack 0.4
                    release 0.9
                    saw-cutoff 300
                    noise-cutoff 100
                    wave 1]
  (let [trg (in:kr beat-trg-bus)
        cnt (in:kr beat-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        attack (buf-rd:kr 1 attack-buf cnt)
        release (buf-rd:kr 1 release-buf cnt)
        b-amp (buf-rd:kr 1 amp-buf cnt)
        gate-trg (and (> note 0) trg)
        freq (midicps note)
        noize (* noise-level (pink-noise))
        wave (select:ar wave [(mix [(lpf (lf-saw freq) saw-cutoff) (lf-tri freq)])
                              (lpf (saw freq) saw-cutoff)
                              (lpf (pulse freq) saw-cutoff)
                              (mix [(lpf (saw freq) saw-cutoff) (pulse freq)])
                              (sum [(lpf (saw (/ freq 2)) saw-cutoff) (lf-tri freq)])])
        src (mix [wave
                  (lpf noize noise-cutoff)])
        src (g-verb src 200 1 0.2)
        e (env-gen (perc attack release) :gate gate-trg)
        amp (+ (* amp 5) amp)]
    (* (* b-amp amp) e src)))


(definst deep-basz [amp 1
                    notes-buf 0
                    noise-level 0.05
                    beat-trg-bus (:beat time/beat-4th)
                    beat-bus     (:count time/beat-4th)
                    attack 0.4
                    release 0.9
                    saw-cutoff 300
                    noise-cutoff 100
                    wave 1]
  (let [trg (in:kr beat-trg-bus)
        cnt (in:kr beat-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        gate-trg (and (> note 0) trg)
        freq (midicps note)
        noize (* noise-level (pink-noise))
        wave (select:ar wave [(mix [(lpf (lf-saw freq) saw-cutoff) (lf-tri freq)])
                              (lpf (saw freq) saw-cutoff)
                              (lpf (pulse freq) saw-cutoff)
                              (mix [(lpf (saw freq) saw-cutoff) (pulse freq)])
                              (sum [(lpf (saw (/ freq 2)) saw-cutoff) (lf-tri freq)])])
        src (mix [wave
                  (lpf noize noise-cutoff)])
        src (g-verb src 200 1 0.2)
        e (env-gen (perc attack release) :gate gate-trg)
        amp (+ (* amp 5) amp)]
    (* amp e src)))

(do
  (def grumble-chord-group
    (do
      (defonce note1-b (buffer 256))
      (defonce note2-b (buffer 256))
      (defonce note3-b (buffer 256))
      (defonce note4-b (buffer 256))

      (defonce grumblers-g (group "the grumblers"))

      (kill wobbling)
      [(wobbling [:head grumblers-g] :notes-buf note1-b :amp 0 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.0)
       (wobbling [:head grumblers-g] :notes-buf note2-b :amp 0 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.0)
       (wobbling [:head grumblers-g] :notes-buf note3-b :amp 0 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.0)
       (wobbling [:head grumblers-g] :notes-buf note4-b :amp 0 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.0)]))

  (def grumble-chords
    (do
      (let [_ [0 0 0]
            [F21 F22 F23 F24 F25 F26 F27] (map #(chord-degree %1 :F1 :minor 3) [:i :ii :iii :iv :v :vi :vii])
            [F31 F32 F33 F34 F35 F36 F37] (map #(chord-degree %1 :F3 :minor 3) [:i :ii :iii :iv :v :vi :vii])
            [F41 F42 F43 F44 F45 F46 F47] (map #(chord-degree %1 :F4 :minor 4) [:i :ii :iii :iv :v :vi :vii])
            [Fa31 Fa32 Fa33 Fa34 Fa35 Fa36 Fa37] (map #(chord-degree %1 :F3 :minor 4) [:i :ii :iii :iv :v :vi :vii])
            [Fa41 Fa42 Fa43 Fa44 Fa45 Fa46 Fa47] (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])

            [C41 C42 C43 C44 C45 C46 C47] (map #(chord-degree %1 :C4 :minor3) [:i :ii :iii :iv :v :vi :vii])
            [C31 C32 C33 C34 C35 C36 C37] (map #(chord-degree %1 :C3 :minor3) [:i :ii :iii :iv :v :vi :vii])]

        (pattern! note1-dur-b [12 12 12 1/2 12 12 12 1/2])
        (let [chord-pat
              (concat

               (repeat 12 [0]) [F22 F21  _ _]
               (repeat 12 [0]) [_ _  _ _]


               ;;[_   _ _ _ _ _ _ _]

               ;; [F23 F21 _ _ F23 F21 _ _]
               ;; [_   F21 _ _ F23 F21 _ _]
               ;; [F24 F21 _ _ F24 F21 _ _]
               ;; [_   F21 _ _ F24 F21 _ _]
               )]
          (let [chord-bufs (shuffle [note1-b note2-b note3-b note4-b])] ;; Play around with some random inversions
            (dotimes [chord-idx (count chord-bufs)]
              (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))))
    ))


(comment
  (map #(ctl %1 :saw-cutoff 800) slow-deep-chord-group)
  (stop)
  (map #(ctl %1 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)
  (map #(ctl %1 :saw-cutoff 800 :release 6 :noise 100.2 :attack 0.4 :amp 0.05) slow-deep-chord-group)

  ;;Drum
  (doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 500 :amp 0.06 :attack 0.01 :noise-level 0 :release 0.5 :beat-trg-bus (:beat time/beat-4th) :wave 4 :beat-bus (:count time/beat-4th)))

  (doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 200 :amp 0.1 :attack 0.1 :noise-level 0.05 :release 1.0 :beat-trg-bus (:beat time/beat-2th) :wave 4 :beat-bus (:count time/beat-2th)))

  (ctl drums-g :mod-index 0.0 :amp 2.2 :mod-freq 0))

(def slow-deep-chord-group
  (do
    ;;(kill deep-basz-pi)
    (defonce sd-g (group "slow deep chords"))
    (defonce sd-note1-b (buffer 256))
    (defonce sd-note2-b (buffer 256))
    (defonce sd-note3-b (buffer 256))
    (defonce sd-note4-b (buffer 256))
    (defonce sd-attack-b (buffer 256))
    (defonce sd-release-b (buffer 256))
    (defonce sd-amp-b (buffer 256))

    [(deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))]))




(defonce w-note-b (buffer 256))
(defonce w-note2-b (buffer 256))

(defonce w-note3-b (buffer 256))
(defonce w-note7-b (buffer 256))

(pattern! w-note7-b [
                     (degrees [1 3 1 3 1 3] :minor :F4)
                     (degrees [3 1] :minor :F4)
                     (degrees [1 4 1 4 1 4] :minor :F4)
                     (degrees [7 7 7 7 7 7] :minor :F3)

                     (degrees [1 4 1 4 1 4] :minor :F4)
                     (degrees [4 1] :minor :F4)
                     (degrees [3 4 3 4 3 4] :minor :F4)

                     (degrees [7 7 7 7 7 7] :minor :F4)
                     (degrees [8 8 8 8 8 8] :minor :F4)
                     (degrees [5 5 5 3 3 3] :minor :F4)
                     ])

(pattern! w-note7-b
          (map #(- % 12)
               (flatten (concat   [(degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                                   (degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                                   (degrees [5] :minor :F3) (degrees [1] :minor :F3) 0 0
                                   (degrees [5] :minor :F4) (degrees [1] :minor :F3) 0 0]

                                  [(degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3)
                                   (degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                                   (degrees [5] :minor :F3) (degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3)
                                   (degrees [6] :minor :F4) (degrees [6] :minor :F3) (degrees [6] :minor :F4) (degrees [5] :minor :F3)]

                                  [(degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                                   (degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                                   (degrees [3] :minor :F3) (degrees [1] :minor :F3) 0 0
                                   (degrees [3] :minor :F4) (degrees [1] :minor :F3) 0 0]

                                  [(degrees [1] :minor :F3) (degrees [3] :minor :F3) (degrees [3] :minor :F3)  (degrees [3] :minor :F3)
                                   (degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                                   (degrees [3] :minor :F3) (degrees [1] :minor :F3) (degrees [3] :minor :F3)  (degrees [3] :minor :F3)
                                   (degrees [3] :minor :F4) (degrees [1] :minor :F3) (degrees [3] :minor :F4)  (degrees [3] :minor :F3)]

                                  [(degrees [1] :minor :F3) (degrees [4] :minor :F3) 0 0
                                   (degrees [1] :minor :F3) (degrees [4] :minor :F3) 0 0
                                   (degrees [4] :minor :F3) (degrees [1] :minor :F3) 0 0
                                   (degrees [4] :minor :F4) (degrees [1] :minor :F3) 0 0]

                                  [(degrees [1] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                                   (degrees [1] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                                   (degrees [4] :minor :F3) (degrees [1] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                                   (degrees [4] :minor :F4) (degrees [5] :minor :F3) (degrees [4] :minor :F4)  (degrees [4] :minor :F4)])))
          )

(comment
  (def apeg-deep (deep-basz :amp 0.7 :noise-level 0.05
                            :notes-buf w-note3-b
                            :beat-trg-bus (:beat time/beat-1th)
                            :beat-bus (:count time/beat-1th)
                            :attack 0.1
                            :release 0.1))
  (ctl apeg-deep :attack 0.5 :release 0.5))


(defonce w-note5-b (buffer 256))

;;Take the last note of every chord

(map find-note-name (note-in-chords 4 :F3 :minor))

(let [[c31 c32 c33 c34 c35 c36 c37] (note-in-chords 4 :F3 :minor)
      [c41 c42 c43 c44 c45 c46 c47] (note-in-chords 4 :F4 :minor)
      [c21 c22 c23 c24 c25 c26 c27] (note-in-chords 4 :F2 :minor)

      [ci21 ci22 ci23 ci24 ci25 ci26 ci27] (note-in-chords 3 :F2 :minor)
      [ci31 ci32 ci33 ci34 ci35 ci36 ci37] (note-in-chords 3 :F3 :minor)
      [ci41 ci42 ci43 ci44 ci45 ci46 ci47] (note-in-chords 3 :F4 :minor)
      ]

  (pattern! w-note6-b
            (repeat 32 [0])
            (repeat 16 [ci21])
            (repeat 16 [c24])

            (repeat 8 [ci23 0 0 0])
            (repeat 16 [c23])
            (repeat 16 [c25])

            (repeat 8 [ci33 0 0  0])
            (repeat 16 [c31])
            (repeat 16 [c34])

            (repeat 8 [ci33 0 0 0])
            (repeat 16 [c33])
            (repeat 16 [c35])

;;            (repeat 8 [ci33 0 0 0])
  ;;          (repeat 16 [c35])
    ;;        (repeat 16 [c37])

      ;;      (repeat 8 [ci34 0 0 0])
        ;;    (repeat 16 [c37])
          ;;  (repeat 16 [c41])

         ;;   (repeat 8 [ci41 0 0 0])
          ;;  (repeat 16 [c41])
          ;;  (repeat 16 [c43])
            )

  (pattern! w-note5-b
             ;; [c35 c31 c33 c33 c33 c33 c32 c31 c33 c33 c33 c33 c33 c33 c33 c32]
             ;; [c31 c31 c31 c31 c31 c31 c31 c31]
             ;; [c34 c34 c34 c31 c34 c34 c34 c31]

            ;;  [c33 c33 c33 c33 c33 c33 c33 c34 c33 c33 c33 c33 c33 c33 c33 c34]
            ;;  [c31 c31 c31 c31 c31 c31 c31 c31]
            ;;  [c34 c34 c34 c31 c34 c35 c35 c31]

            [c25 c21 c23 c23 c23 c23 c23 c24  c23 c23 c23 c23 c23 c23 c23 c22]
            [c21 c21 c21 c21 c21 c21 c21 c21]
            [c24 c24 c24 c21 c24 c24 c24 c21]

            [c23 c23 c23 c23 c23 c23 c23 c24 c23 c23 c23 c23 c23 c23 c23 c24]
            [c21 c21 c21 c21 c21 c21 c21 c21]
            [c24 c24 c24 c21 c24 c25 c25 c21]
            ))

;;(map find-note-name [53 60 51 56 53 60 51 56 53 60 51 56 53 60 51 56])
;;(:F3 :C4 :Eb3 :Ab3 :F3 :C4 :Eb3 :Ab3 :F3 :C4 :Eb3 :Ab3 :F3 :C4 :Eb3 :Ab3)

(do
;;  (kill apeg-deep3)

  (def apeg-deep-fast (deep-basz :amp 0.0 :noise-level 0.05
                             :notes-buf w-note6-b
                             :beat-trg-bus (:beat time/beat-1th)
                             :beat-bus     (:count time/beat-1th)
                             :attack 0.1
                             :release 0.1))

  (def apeg-deep-slow (deep-basz :amp 0.0 :noise-level 0.05
                             :notes-buf w-note5-b
                             :beat-trg-bus (:beat time/beat-2th)
                             :beat-bus (:count time/beat-2th)
                             :attack 0.5
                             :wave 5
                             :saw-cutoff 300
                             :release 0.5))

  )

(def with-chords
  (let [chord-bufs (repeatedly 4 #(buffer 256))
        synths  (repeatedly 4 #(deep-basz :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.5 :wave 4 :release 0.5))]
    (doseq [[s idx] (map vector synths (range))] (println s)
           (ctl s :notes-buf (nth chord-bufs idx)))
    [synths chord-bufs]))

(map #(kill %1) (first with-chords))

(let [bufs (second with-chords)
;;    [c31 c32 c33 c34 c35 c36 c37] (chords-for :F3 :minor 4)
      [ci31 ci32 ci33 ci34 ci35 ci36 ci37]        (chords-with-inversion [1] :C3 :minor :up)
      [cii31 cii32 cii33 cii34 cii35 cii36 cii37] (chords-with-inversion [1 2] :C3 :minor :up)
;;    [civ31 civ32 civ33 civ34 civ35 civ36 civ37] (chords-with-inversion [4] :F3 :minor :down)

      chord-pat [ci35 ci31 ci33 ci33 ci33 ci33 ci32 ci31 ci33 ci33 ci33 ci33 ci33 ci33 ci33 ci32
                 ci31 ci31 ci31 ci31 ci31 ci31 ci31 ci31
                 ci34 ci34 ci34 ci31 ci34 ci34 ci34 ci31

                 cii33 cii33 cii33 cii33 cii33 cii33 cii33 cii34 cii33 cii33 cii33 cii33 cii33 cii33 cii33 cii34
                 cii31 cii31 cii31 cii31 cii31 cii31 cii31 cii31
                 cii34 cii34 cii34 cii31 cii34 cii35 cii35 cii31]]
  (dotimes [chord-idx (count bufs)]
    (pattern! (nth bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))

(let [bufs (second with-chords)
      [c31 c32 c33 c34 c35 c36 c37]                      (chords-for :F3 :minor 4)
      [cii21 cii22 cii23 cii24 cii25 cii26 cii27]        (chords-with-inversion [1 2] :F2 :minor :up)
      [ci21 ci22 ci23 ci24 ci25 ci26 ci27]               (chords-with-inversion [1] :F2 :minor :up)
      [ci31 ci32 ci33 ci34 ci35 ci36 ci37]               (chords-with-inversion [1] :F3 :minor :up)
      [cii31 cii32 cii33 cii34 cii35 cii36 cii37]        (chords-with-inversion [1 2] :F3 :minor :up)

      chord-pat (concat (repeat 32 c31)
                        (repeat 8 [c31 c33])
                        (repeat 8 [c34 c31])

                        (repeat 32 cii26)
                        (repeat 8 [cii25 cii23])
                        (repeat 8 [cii24 cii21])

                        (repeat 32 cii26)
                        (repeat 8 [cii21 cii23])
                        (repeat 8 [cii24 cii21])

                        (repeat 64 [0])
                        )]
  (dotimes [chord-idx (count bufs)]
        (pattern! (nth bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))



(map find-note-name (flatten (concat [29 32 48 51] [31 34 49 53] [32 36 51 55] [34 37 53 56] [36 39 55 58] [37 41 56 60] [39 43 58 61])))
[:F1 :Ab1 :C3 :Eb3
 :G1 :Bb1 :C#3 :F3
 :Ab1 :C2 :Eb3 :G3
 :Bb1 :C#2 :F3 :Ab3
 :C2 :Eb2 :G3 :Bb3
 :C#2 :F2 :Ab3 :C4
 :Eb2 :G2 :Bb3 :C#4]

(map find-note-name (concat [53 44 48] [55 46 49] [56 48 51] [58 49 53] [60 51 55] [61 53 56] [63 55 58]))

[:F3 :Ab2 :C3
 :G3 :Bb2 :C#3
 :Ab3 :C3 :Eb3
 :Bb3 :C#3 :F3
 :C4 :Eb3 :G3
 :C#4 :F3 :Ab3
 :Eb4 :G3 :Bb3]

(map find-note-name (flatten [[41 44 48 51] [43 46 49 53] [44 48 51 55] [46 49 53 56] [48 51 55 58] [49 53 56 60] [51 55 58 61]]))

[:F2 :Ab2 :C3 :Eb3
 :G2 :Bb2 :C#3 :F3
 :Ab2 :C3 :Eb3 :G3
 :Bb2 :C#3 :F3 :Ab3
 :C3 :Eb3 :G3 :Bb3
 :C#3 :F3 :Ab3 :C4
 :Eb3 :G3 :Bb3 :C#4]


;;[56 48 51]



(pattern!
 w-note3-b
 [;;(degrees [2 3 4 5] :minor :F3)
  (repeat (* 2 4)  [(degrees [3 1 1 1] :minor :F3)])
  (repeat (* 2 4)  [(degrees [4 1 1 1] :minor :F3)])
  ;;(repeat (* 2 8)  (degrees [1] :minor :F3))

;;  (degrees [1 1 1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3) ;;(degrees [1 1] :minor :F3) (degrees [1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3)
;;  (degrees [1 1 1 1] :minor :F3)

  (degrees [3 4 4 1] :minor :F3)

  ;;(degrees [3 3 3 3] :minor :F3)
;;  (repeat 4 (degrees [5] :minor :F3))
  (repeat (* 1 2) (degrees [5 1] :minor :F3))
  (repeat (* 1 2) (degrees [5 4] :minor :F3))
  ;;(degrees [3 3 3 3] :minor :F3)
  ;;(degrees [3 3 3 3] :minor :F3)
  ;;(degrees [3 3 3 3] :minor :F3) ;;(degrees [3 3] :minor :F3)

  (repeat (* 3 2) (degrees [5 1] :minor :F3))
  (repeat (* 3 1) (degrees [4 4] :minor :F3))
  (repeat (* 3 1) (degrees [5 5] :minor :F3))

;;  (degrees [3 3 3 3] :minor :F3)
;;  (degrees [3 3 3 3] :minor :F3)
;;  (degrees [3 3 3 3] :minor :F3)
  ])


(do
  (comment  (def slow-deep (deep-basz :amp 0.7
                                      :noise-level 0.05
                                      :notes-buf w-note-b
                                      :beat-trg-bus (:beat time/beat-4th)
                                      :beat-bus (:count time/beat-4th)
                                      :saw-cutoff 0
                                      :attack 0.4
                                      ))

            ;;(ctl slow-deep :saw-cutoff 200)
            ;;(ctl highlight-deep :saw-cutoff 500)
            ;;(kill highlight-deep)

            (ctl highlight-deep :saw-cutoff 10 :wave 0)
            (def highlight-deep (deep-basz :amp 0.9 :noise-level 0.05
                                           :notes-buf w-note2-b
                                           :beat-trg-bus (:beat time/beat-1th)
                                           :beat-bus (:count time/beat-1th)
                                           :attack 0.1
                                           :release 0.1
                                           )))


;;:F3 :Ab2 :C3 :G3 :Bb2 :C#3 :Ab3 :C3 :Eb3 :Bb3 :C#3 :F3 :C4 :Eb3 :G3 :C#4 :F3 :Ab3 :Eb4 :G3 :Bb3
(map find-note-name (flatten (concat [53 44 48] [55 46 49] [56 48 51] [58 49 53] [60 51 55] [61 53 56] [63 55 58])))

;;(map find-note-name (degrees [3] :minor :F3)) => f3


  (pattern! w-note3-b [[(degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                        (degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                        (degrees [5] :minor :F3) (degrees [1] :minor :F3) 0 0
                        (degrees [5] :minor :F4) (degrees [1] :minor :F3) 0 0]

                       [(degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3)
                        (degrees [1] :minor :F3) (degrees [5] :minor :F3) 0 0
                        (degrees [5] :minor :F3) (degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3)
                        (degrees [6] :minor :F4) (degrees [6] :minor :F3) (degrees [6] :minor :F4) (degrees [5] :minor :F3)]


                       [(degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                        (degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                        (degrees [3] :minor :F3) (degrees [1] :minor :F3) 0 0
                        (degrees [3] :minor :F4) (degrees [1] :minor :F3) 0 0]

                       [(degrees [1] :minor :F3) (degrees [3] :minor :F3) (degrees [3] :minor :F3)  (degrees [3] :minor :F3)
                        (degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0
                        (degrees [3] :minor :F3) (degrees [1] :minor :F3) (degrees [3] :minor :F3)  (degrees [3] :minor :F3)
                        (degrees [3] :minor :F4) (degrees [1] :minor :F3) (degrees [3] :minor :F4)  (degrees [3] :minor :F3)
                        ]


                       [(degrees [1] :minor :F3) (degrees [4] :minor :F3) 0 0
                        (degrees [1] :minor :F3) (degrees [4] :minor :F3) 0 0
                        (degrees [4] :minor :F3) (degrees [1] :minor :F3) 0 0
                        (degrees [4] :minor :F4) (degrees [1] :minor :F3) 0 0]

                       [(degrees [1] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                        (degrees [1] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                        (degrees [4] :minor :F3) (degrees [1] :minor :F3) (degrees [4] :minor :F3)  (degrees [4] :minor :F3)
                        (degrees [4] :minor :F4) (degrees [5] :minor :F3) (degrees [4] :minor :F4)  (degrees [4] :minor :F4)]

                       ])
  )

(map #(map find-note-name %1) (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii]))

(let [_ [0 0 0 0]
      [c21 c22 c23 c24 c25 c26 c27]        (chords-for :C2 :minor 3)
      [f21 f22 f23 f24 f25 f26 f27]        (chords-for :C2 :minor 3)
      [fm21 fm22 fm23 fm24 fm25 fm26 fm27] (chords-for :F2 :major 4)
      [f31 f32 f33 f34 f35 f36 f37]        (chords-for :F3 :minor 3)
      [f41 f42 f43 f44 f45 f46 f47]        (chords-for :F4 :minor 3)

      [fii21 fii22 fii23 fii24 fii25 fii26 fii27]        (chords-with-inversion [1 2] :F2 :minor :down)
      [fi21 fi22 fi23 fi24 fi25 fi26 fi27]               (chords-with-inversion [1]   :F2 :minor :down)
      [fi31 fi32 fi33 fi34 fi35 fi36 fi37]               (chords-with-inversion [1]   :F3 :minor :down)
      [fii31 fii32 fii33 fii34 fii35 fii36 fii37]        (chords-with-inversion [1 2] :F3 :minor :down)
      ]
  (let [chord-pat
        [fii21 fii21 fii21 fii21 fii21 fii21 fii21 fii21

         fii24 fii24 fii24 fii24 fii24 fii24 fii24 fii24
         fii25 fii25 fii25 fii25 fii25 fii25 fii25 fii25

         fii26 fii26 fii26 fii26 fii26 fii26 fii26 fii26

         f31 f31 f31 f31         f31 f31 f31 f31

         f34 f34 f34 f34 f34 f34 f34 f34

         f35 f35 f35 f35 f34 f34 f34 f34

         f36 f36 f36 f36 f34 f34 f34 f34
         ]]
    (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b]]
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))



(def chords-score
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27]        (chords-for :C2 :minor 3)
        [f217 f227 f237 f247 f257 f267 f277] (chords-for :F2 :melodic-minor-asc 4)
        [fm21 fm22 fm23 fm24 fm25 fm26 fm27] (chords-for :F2 :major 4)
        [f31 f32 f33 f34 f35 f36 f37]        (chords-for :F3 :minor 3)
        [f41 f42 f43 f44 f45 f46 f47]        (chords-for :F4 :minor 3)
        [fu21 fu22 fu23 fu24 fu25 fu26 fu27]          (chords-with-inversion [1] :F2 :minor :up 3)
        [fuu21 fuu22 fuu23 fuu24 fuu25 fuu26 fuu27]   (chords-with-inversion [1 2] :F2 :minor :up 3)
        [f3ii21 f3ii22 f3ii23 f3ii24 f3ii25 f3ii26 f3ii27]   (chords-with-inversion [1 2] :F2 :minor :down 3)
        [fii217 fii227 fii237 fii247 fii257 fii267 fii277]   (chords-with-inversion [1 2] :F2 :minor :down 4)
        [fii21 fii22 fii23 fii24 fii25 fii26 fii27]   (chords-with-inversion [1 2] :F2 :minor :down 3)
        [fi11 fi12 fi13 fi14 fi15 fi16 fi17]          (chords-with-inversion [1 2] :F2 :minor :down)
        [fi21 fi22 fi23 fi24 fi25 fi26 fi27]          (chords-with-inversion [1]   :F2 :minor :down)
        [f21 f22 f23 f24 f25 f26 f27]                 (chords-for :F2 :minor 3)
        [fmm21 fmm22 fmm23 fmm24 fmm25 fmm26 fmm27]   (chords-with-inversion [1] :F2 :melodic-minor :down)
        [fi31 fi32 fi33 fi34 fi35 fi36 fi37]          (chords-with-inversion [1] :F3 :minor :down)
        [fii31 fii32 fii33 fii34 fii35 fii36 fii37]   (chords-with-inversion [1 2] :F3 :minor :down)

        f317 (first (chords-for :F3 :minor 4))
        f257 (first (chords-for :F2 :minor 4))

        f41dim (chord :F3 :a)
        f42dim (chord :G2 :7sus4)
        f43dim (chord :A3 :dim)
        f44dim (chord :F3 :7sus4)
        f45dim (chord :C3 :7sus4)
        f46dim (chord :D3 :7sus4)
        f47dim (chord :E3 :7sus4)

        sus4 (chord :F3 :dim)

        [fma21 fma22 fma23 fma24 fma25 fma26 fma27] (chords-with-inversion [] :F2 :melodic-minor-asc :up 3)
        [fmd21 fmd22 fmd23 fmd24 fmd25 fmd26 fmd27] (chords-with-inversion [] :F2 :melodic-minor-desc :up 3)
        ;;      [fma21 fma22 fma23 fma24 fma25 fma26 fma27] (chords-for :F2 :melodic-minor)

        all (chord-degree :ii :F3 :melodic-minor-asc)

        ]
    (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
          _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
          _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])

          chord-pat
          [
           ;;         f3i f


           fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
           fu23 fu23 fu23 fu23 fu23 fu23 fu23 fu23
           fu23 fu23 fu23 fu23 fu23 fu23 fu23 fu23

           ;; fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
           ;; fu24 fu24 fu24 fu24 fu24 fu24 fu24  fu24 fu24 fu24 fu24 fu24 fu24 fu24 fu24 fu24

           ;; fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
           ;; f25 f25 f25 f25 f25 f25 f25  f25 f25 f25 f25 f25 f25 f25 f25 f25

           ;; f23 f23 f23 f23 f23 f23 f23 f23
           ;; f24 f24 f24 f24 f24 f24 f24 _

           ;; fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21 fu21
           ;; f26 f26 f26 f26 f26 f26 f26 f26
           ;; f25 f25 f25 f25 f25 f25 f25 _

           ;; fu21 _ fu21  fu23 fu23  fu21 fu21    fu25 fu25
           ;; fu25 fu25 fu25 fu25 fu25 fu25 fu25 fu25
           ;; f26 f26 f26 f26 f26 f26 f26 f26
           ;; f25 f25 f25 f25 f25 f25 f25 _

           ;; fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
           ;; fi23 fi23 fi23 fi23
           ;; fii237 fii237 fii237 fi23

           ;; f247 f247 f247 f247 f247 f247 f247 f247
           ;; fi23 fi23 fi23 fi23
           ;; fii237 fii237 fii237 fi23

           ;; fi25 fi25 fi25 fi25 fi25 fi25 fi25 fi25
           ;; fi24 fi24 fi24 fi24
           ;; fi24 fi24 fi24 fi24

           ;; ;; ;;--

           ;; fii267 fii267 fii267 fii267 fii267 fii267 fii267 fii267
           ;; fi25 fi25 fi25 fi25
           ;; fi25 fi25 fi25 fi25

           ;; fmm25 fmm25 fmm25 fmm25 fmm25 fmm25 fmm25 fmm25
           ;; f31 f31 f31 f31
           ;; f31 f31 f31 f31

           ;; fii26 fii26 fii26 fii26 fii26 fii26 fii26 fii26
           ;; f31 f31 f31 f31
           ;; f31 f31 f31 f31

           ;;fmm27 fmm27 fmm27 fmm27 fmm27 fmm27 fmm27 fmm27 fmm27
           ;;f24 f24 f24 f24 f24 f24 f24 f24

           ;; f24 f24 f24 f24 f24 f24 f24 f24
           ;; fi23 fi23 fi23 fi23 fi23
           ;; f23 f23 f23

           ;;Full crazy

           ;; fi34 fi34 fi34 fi34 fi34 fi34 fi34 fi34
           ;; fi33 fi33 fi33 fi33 fi33
           ;; fii33 fii33 fii33

           ;; fi35 fi35 fi35 fi35 fi35 fi35 fi35 fi35
           ;; fi34 fi34 fi34 fi34 fi34
           ;; fi34 fi34 fi34

           ;; fii36 fii36 fii36 fii36 fii36 fii36 fii36 fii36
           ;; fi33 fi33 fi33 fi33 fi33
           ;; fii31 fii31 fii31

           ;; fii36 fii36 fii36 fii36 fii36 fii36 fii36 fii36
           ;; fii35 fii35 fii35 fii35 fii36 fii36 fii36 fii36

           ;; fi21 fi21 fi21 fi21
           ;; fi26 fi26 fi26 fi26
           ;; fi27 fi27 fi27 fi27
           ;; fi21 fi21 fi21 fi21
           ;; fi21 fi21 fi21 fi21
           ;; fi23 fi23 fi23 fi23
           ;; fi24 fi24 fi24 fi24
           ;; fi25 fi25 fi25 fi25


           ]]
      (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b]]
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))

;;?PART 1?
(pattern! w-note3-b
          (repeat 8 [(degrees [1 3 5 4] :minor :F3)])
          (repeat 5 [(degrees [1 3] :minor :F3) 0 0 ])
          (repeat 3 [(degrees [4 1 1 1] :minor :F3)])

          (repeat 8 [(degrees [6 1 3 5] :minor :F3)])
          (repeat 5 [(degrees [5] :minor :F3)  (degrees [3] :minor :F3) 0 0])
          (repeat 3 [(degrees [4] :minor :F3)  (degrees [1] :minor :F3) 0 0])

          (repeat 8 [(degrees [6 4 2 1] :minor :F3)])
          (repeat 4 [(degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0])
          (repeat 4 [(degrees [4] :minor :F3) (degrees [1] :minor :F3) 0 0])

          (repeat 4 [(degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          (repeat 4 [(degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [2] :minor :F3)])
          (repeat 4 [(degrees [1] :minor :F3) (degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2)])
          (repeat 4 [(degrees [4] :minor :F3) (degrees [6] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          )

;;PART 2
(pattern! w-note3-b
          (repeat 4 [(degrees [1] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          (repeat 4 [(degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [2] :minor :F3)])
          (repeat 4 [(degrees [1] :minor :F3) (degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2)])
          (repeat 4 [(degrees [4] :minor :F3) (degrees [6] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)]))

;;PART 3
(pattern! w-note3-b
          (repeat 4 [(degrees [1] :minor :F4) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          (repeat 4 [(degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [2] :minor :F3)])
          (repeat 4 [(degrees [1] :minor :F4) (degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2)])
          (repeat 4 [(degrees [4] :minor :F3) (degrees [6] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])

          (repeat 4 [(degrees [3] :minor :F4) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          (repeat 4 [(degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2) (degrees [2] :minor :F3)])
          (repeat 4 [(degrees [3] :minor :F4) (degrees [3] :minor :F3) (degrees [5] :minor :F3) (degrees [7] :minor :F2)])
          (repeat 4 [(degrees [4] :minor :F3) (degrees [6] :minor :F3) (degrees [7] :minor :F2) (degrees [3] :minor :F3)])
          )

;;Switch scale
(pattern! w-note3-b
          (repeat 4 [(degrees [1] :major :F4) (degrees [5] :major :F3) (degrees [7] :major :F2) (degrees [3] :major :F3)])
          (repeat 4 [(degrees [3] :major :F3) (degrees [5] :major :F3) (degrees [7] :major :F2) (degrees [2] :major :F3)])
          (repeat 4 [(degrees [1] :major :F4) (degrees [3] :major :F3) (degrees [5] :major :F3) (degrees [7] :major :F2)])
          (repeat 4 [(degrees [4] :major :F3) (degrees [6] :major :F3) (degrees [7] :major :F2) (degrees [3] :major :F3)]))

(pattern-at! w-note3-b time/main-beat 0
             (repeat 4 [(degrees [1] :minor :F3) (degrees [3] :minor :F3) 0 0])
             (repeat 4 [(degrees [4] :minor :F3) (degrees [1] :minor :F3) 0 0])
             (repeat 4 [(degrees [5] :minor :F3) (degrees [3] :minor :F3) 0 0])
             (repeat 4 [(degrees [4] :minor :F3) (degrees [1] :minor :F3) 0 0])
             )

(pattern! w-note3-b
          (repeat 2 [(degrees [1] :major :F4) 0 (degrees [3] :major :F3)])
          (repeat 1 [(degrees [3] :major :F3) 0 (degrees [3] :major :F3)])
          (repeat 2 [(degrees [1] :major :F4) 0 (degrees [4] :major :F3)])

          (repeat 2 [(degrees [3] :major :F4) 0 (degrees [5] :major :F3)])
          (repeat 1 [(degrees [5] :major :F3) 0 (degrees [5] :major :F3)])
          (repeat 2 [(degrees [3] :major :F4) 0 (degrees [5] :major :F3)])

          (repeat 2 [(degrees [7] :major :F3) 0 (degrees [5] :major :F3)])
          (repeat 1 [(degrees [4] :major :F3) 0 (degrees [4] :major :F3)])
          (repeat 2 [(degrees [5] :major :F3) 0 (degrees [5] :major :F3)])

          (repeat 2 [(degrees [4] :major :F4) 0 (degrees [1] :major :F4)])
          (repeat 1 [(degrees [3] :major :F4) 0 (degrees [1] :major :F4)])
          (repeat 2 [(degrees [3] :major :F4) 0 (degrees [1] :major :F4)])
          )

(pattern! w-note3-b (repeat 2 [(degrees [3] :minor :F2)
                               0
                               (degrees [1] :minor :F3)
                               (degrees [4] :minor :F3)
                               0
                               (degrees [1] :minor :F3)
                               (degrees [5] :minor :F3)
                               (degrees [7] :minor :F3)
                               (degrees [5] :minor :F3)
                               (degrees [8] :minor :F3)
                               0
                               ]))

(pattern! w-note2-b
          (repeat 16 [0])
          (repeat 1  [0 0 0 0 (degrees [3] :major :F2) 0 (degrees [3] :major :F2) 0])
          (repeat 1  [0 0 0 0 (degrees [4] :major :F2) 0 (degrees [4] :major :F2) 0])

          (repeat 16 [0])
          (repeat 1  [0 0 0 0 (degrees [3] :minor :F2) 0 (degrees [3] :minor :F2) 0])
          (repeat 1  [0 0 0 0 (degrees [4] :minor :F2) 0 (degrees [4] :minor :F2) 0]))

(pattern! w-note-b
          (repeat 8  (degrees [1] :major :F2))
          (repeat 4  (degrees [3] :minor :F2))
          (repeat 4  (degrees [4] :minor :F2)))

(pattern! w-note-b
          (repeat 1 [(degrees [1] :major :F2)
                     (degrees [3] :major :F2)
                     (degrees [4] :major :F2)])
          )

;;PART 2
(pattern! w-note2-b
          (repeat 8 [(degrees [1] :major :F3)])
          (repeat 8 [0])
          (repeat 1  [0 0 0 0 0 0 0 0])
          (repeat 1  [0 0 0 0 0 0 0 0])

          (repeat 16 [0])
          (repeat 1  [0 0 0 0 0 0 0 0])
          (repeat 1  [0 0 0 0 0 0 0 0])

          (repeat 16 [0])
          (repeat 1  [0 0 0 0 0 0 0 0])
          (repeat 1  [0 0 0 0 0 0 0 0])

          (repeat 16 [0])
          (repeat 1  [0 0 0 0 0 0 0 0])
          (repeat 1  [0 0 0 0 0 0 0 0]))

(pattern! w-note2-b
          (repeat 16 [0])
          (repeat 8  [(degrees [1] :major :F3)])
          (repeat 8  [(degrees [1] :major :F3)])

          (repeat 16 [0])
          (repeat 8  [(degrees [1] :major :F3)])
          (repeat 8  [(degrees [1] :major :F3)])

          (repeat 16 [0])
          (repeat 8  [(degrees [1] :major :F3)])
          (repeat 8  [(degrees [1] :major :F3)])

          (repeat 16 [0])
          (repeat 8  [(degrees [1] :major :F4)])
          (repeat 8  [(degrees [1] :major :F4)])
          )


(pattern! w-note-b
          (repeat 8  [(degrees [1] :minor :F2)])
          (repeat 1  [(degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2)])
          (repeat 1  [(degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2)])

          (repeat 8  [(degrees [1] :minor :F2)])
          (repeat 1  [(degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2)])
          (repeat 1  [(degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2)])

          (repeat 8  [(degrees [1] :minor :F2)])
          (repeat 1  [(degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2)])
          (repeat 1  [(degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2)])

          (repeat 8  [(degrees [3] :minor :F2)])
          (repeat 1  [(degrees [3] :minor :F2) (degrees [3] :minor :F2) (degrees [3] :minor :F2)  (degrees [3] :minor :F2)])
          (repeat 1  [(degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2) (degrees [4] :minor :F2)])
          )

;;INIT
(pattern! w-note2-b
          (repeat 16 [0])
          (repeat 1  [0 0 0 0 (degrees [3] :major :F2) 0 (degrees [3] :major :F2) 0])
          (repeat 1  [0 0 0 0 (degrees [4] :major :F2) 0 (degrees [4] :major :F2) 0])

          (repeat 16 [0])
          (repeat 1  [0 0 0 0 (degrees [3] :minor :F2) 0 (degrees [3] :minor :F2) 0])
          (repeat 1  [0 0 0 0 (degrees [4] :minor :F2) 0 (degrees [4] :minor :F2) 0]))

(pattern! w-note-b
          (repeat 8  [(degrees [1] :major :F2)])
          (repeat 1  [(degrees [3] :major :F2) (degrees [3] :major :F2) 0 0])
          (repeat 1  [(degrees [4] :major :F2) (degrees [4] :major :F2) 0 0])

          (repeat 8  [(degrees [1] :minor :F2)])
          (repeat 1  [(degrees [3] :minor :F2) (degrees [3] :minor :F2) 0 0])
          (repeat 1  [(degrees [4] :minor :F2) (degrees [4] :minor :F2) 0 0]))


;;(stop)

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
          src (lpf (mix [noize (pulse:ar freq 0.01)]) 2000)
          src (pitch-shift src 0.01 0.9 1 0.1)
          e (env-gen (adsr :attack 0.6 :release 2.0 :sustain 2.0) :gate gate-trg)]
      (* (+ (* amp 1) amp) e src)))

  (kill sawy)
  (sawy :noise-level 0.1 :amp 0.6)
  (pattern! s-note-b
            [(degrees [1] :minor :F1) (degrees [3] :minor :F1) 0 (degrees [4] :minor :F1) (degrees [1] :minor :F1) 0 0 0]
            (repeat 24 [0])
;;            (repeat (* 3 8) [0])
;;            (repeat (* 3 8) [0])
)
)

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf bass-notes2-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

(do
  (pattern! effects2-seq-buf [1 1 0 0 0 0 0 0])
  (pattern! effects2-seq-buf [1 1 1  1 0 0  0 1 0  0 0 0  0 0 0])
  (pattern! effects2-seq-buf [1 0 0  1 1 1  0 0 0  1 0 0])
  (pattern! effects-seq-buf  (repeat 12 1)  [1 0 0 0])
  (pattern! effects2-seq-buf (repeat 12 0) [1 0 0 0] (repeat 16 0) (repeat 16 0))

  (kill seqer)
  (def hand-drums (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.3 :num-steps 16 :buf hand-drum-s :rate-start 1.0 :rate-limit 1.0) (range 0 16))))

  (def clap2-drums (doall (map #(seqer [:head drum-effects-g]
                                       :rate-start 0.5 :rate-limit 0.6
                                       :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))

  (def clap-drums  (doall (map #(seqer [:head drum-effects-g]
                                       :rate-start 0.5 :rate-limit 0.6
                                       :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16)))))

(kill drum-effects-g)
(kill drums-g)

(pattern! bass-notes-buf (repeat 16 [:F2]) (repeat 16 [:G2]))
(pattern! bass-notes2-buf (repeat 16 [:F2]) (repeat 16 [:G2]))

(pattern! bass-notes-buf
          (repeat 7 [(degrees [1] :minor :F2) 0 0 0])
          (repeat 7 [(degrees [2] :minor :F2) 0 0 0])
          [0 0 0 0]
          [0 (degrees [1] :minor :F2) (degrees [3] :minor :F2) (degrees [4] :minor :F2)])

(do
  (pattern! hats-buf      [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])
  (pattern! kick-seq-buf  [1 0 0 1 0 0 0 0   1 0 0 0 0 0 0 0])

  (def white (doall (map #(whitenoise-hat [:head drums-g] :amp 1.0 :seq-buf hats-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 24 :release 0.1 :attack 0.0 :beat-num %1) (range 0 24))))
  (ctl white :attack 0.01 :release 0.07 :amp 2)

  (def kicker (doall (map #(space-kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 32 :beat-num %1 :noise 0.05 :amp 4.2 :mod-index 0.1 :mod-freq 4.0 :mode-freq 0.2) (range 0 32))))
  (ctl kicker :attack 0.0 :sustain 0.2 :amp 1.0)
    (map #(ctl %1 :t 0.005 :amp 0.35) grumble-chord-group)
  )

(map #(ctl %1 :saw-cutoff 1000 :noise-level 0.5 :amp 0.09 :attack 0.3 :release 6.0 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)
(map #(ctl %1 :t 0.005 :amp 0.4) grumble-chord-group)
(map #(ctl %1 :saw-cutoff 900) slow-deep-chord-group)

(pattern! hats-buf      [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])
(pattern! kick-seq-buf  [1 0 0 1 0 0 0 0   1 0 0 0 0 0 0 0])

(pattern! kick-seq-buf [1 0 0 0 0 0 0 0])
(pattern! kick-seq-buf [1 0 0 0])
(pattern! kick-seq-buf [1 0 0])
(pattern! kick-seq-buf [1 0])
(pattern! kick-seq-buf [1 0])
(pattern! hats-buf [1])

(pattern! bass-notes-buf
          (repeat 8 [(degrees [1] :minor :F2)])
          (degrees [1] :minor :F2) (degrees [3] :minor :F2) (repeat 6 [(degrees [1] :minor :F2)])
          (repeat 8 [(degrees [1] :minor :F2)])
          (degrees [1] :minor :F2) (degrees [2] :minor :F2) (repeat 6 [(degrees [1] :minor :F2)]))

(pattern! kick-seq-buf
          [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 0 0]
          [1 0 0 0 0 0 0 0] [1 0 0 0 1 0 0 0] [1 0 0 0 0 0 0 0] [1 0 0 0 1 1 1 1])

(pattern! kick-seq-buf
          [1 0 0 0 1 0 0 0] [1 1 0 0 1 0 0 0] [1 0 0 0 1 0 0 0] [1 1 0 0 1 0 0 0]
          [1 0 0 0 1 0 0 0] [1 1 0 0 1 0 0 0] [1 0 0 0 1 0 0 0] [1 1 0 0 1 0 0 0])

(pattern! hats-buf
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 1 0 0 1 0]
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 1 0 0 1 0])

(pattern! hats-buf
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0]
          [1 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [1 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0])

(pattern! bass-notes-buf
          (repeat 8 (degrees [1] :minor :F1))
          (repeat 2 (repeat 8 (degrees [1] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          [(degrees [1 1 1 1  5 4 3 1] :minor :F1)])


(defonce w-note8-b (buffer 256))
(defonce w-note9-b (buffer 256))
(defonce w-note10-b (buffer 256))

(def apeg-deep-melody
  [(deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note3-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
    (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note8-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
    (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note9-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
    (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note10-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)])

;;(ctl apeg-deep-melody :amp 0.05 :saw-cutoff 1500 :wave 3 :attack 0.01 :release 1.0)
(map #(ctl % :saw-cutoff 2500 :wave 4) apeg-deep-melody)
(map #(do (ctl % :amp 0.00 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0)
          (n-overtime! % :amp 0 0.04)) apeg-deep-melody)

(ctl apeg-deep-melody :amp 0.06 :saw-cutoff 1000 :wave 1 :attack 1.0 :release 5.0)
(set-beat apeg-deep-melody time/beat-1th)

(pattern! w-note8-b
          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]

          [0 (degrees [1] :minor :F3) (degrees [7] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [7] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]
          )

(pattern! w-note3-b
          [(degrees [4] :minor :F4) 0 0 0 (degrees [6] :minor :F4) 0 0 0]
          [(degrees [7] :minor :F3) 0 0 0 (degrees [6] :minor :F3) 0 0 0]

          [(degrees [5] :minor :F3) 0 0 0 (degrees [6] :minor :F3) 0 0 0
           (degrees [5] :minor :F3) 0 0 0 (degrees [6] :minor :F3) 0 0 0
           ]
          )

(def pinger-score
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27]        (chords-for :C2 :minor 1)
        [c31 c32 c33 c34 c35 c36 c37]        (chords-for :C3 :minor 1)
        [f21 f22 f23 f24 f25 f26 f27]        (chords-for :F2 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37]        (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47]        (chords-for :F4 :minor 1)
        ]
    (let [chord-pat
          [f41 f43 f41 f44 c37 c36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])
           ]]
      (let [chord-bufs [w-note3-b w-note8-b w-note9-b w-note10-b]]
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))


(n-overtime! apeg-deep-melody :amp 0.0 0.4)

(comment
  (ctl apeg-deep-fast :amp 0.2 :saw-cutoff 200)
  (ctl apeg-deep-slow :amp 0.2 :saw-cutoff 200)
  )

(do
  (doseq [s slow-deep-chord-group] (n-overtime! s :amp 0.05 0.05))
  (ctl apeg-deep-fast :notes-buf w-note3-b)
  (ctl apeg-deep-slow :amp 0)
  (ctl apeg-deep-fast :amp 0.8)
  (ctl drum-effects-g :amp 0)
  (ctl drums-g :amp 0)

  (map #(ctl %1 :amp 0.0 :noise-level 0.9 :noise-cutoff 1000 :saw-cutoff 300 :noise-level 0 :wave 4 :release 0.2 :attack 0.2) (first with-chords))
  (ctl apeg-deep-fast :notes-buf w-note3-b)
  (ctl apeg-deep-fast :saw-cutoff 200 :wave 5 :amp 0.06)
  )

(do
  (mono-player (dirt :pad 0))
  (pattern! kick-seq-buf [1 0 0 0 0 0 0 0])
  (doseq [s slow-deep-chord-group] (ctl s :amp 0.05 :saw-cutoff 1500))
  (ctl drum-effects-g :amp 0.3)
  (ctl drums-g :amp 2.0)

  (map #(ctl %1 :amp 0.0 :noise-level 0.9 :noise-cutoff 5000 :saw-cutoff 1000 :noise-level 0 :wave 0 :release 0.2 :attack 0.2) (first with-chords))

;;  (kill apeg-bass-wallop)

  (def apeg-bass-wallop (deep-basz :amp 0.7 :noise-level 0.05 :notes-buf w-note7-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1))
  )

(do
  (when (resolve 'apeg-melody)
    (ctl (resolve 'apeg-melody) :saw-cutoff 1000 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :release 6))
  )

;;(ctl (foundation-output-group) :master-volume 1)
;;(stop)
;;(kill grumblers-g)
;;(remove-all-beat-triggers)

;;(on-beat-trigger 256 #(echoey-buf pulse-s :amp 0.02))
;;(on-beat-trigger 64 #(echoey-buf godzilla-s :amp 0.3))
;;(on-beat-trigger 64 #(spacy constant-blues-s :amp 0.5))

;;(on-beat-trigger 32 #(spacy (dirt :kurt 1)))
;;(on-beat-trigger 64 #(spacy (dirt :kurt 3)))
;;(on-beat-trigger 128 #(spacy (dirt :kurt 2)))

;;(sample-trigger #(spacy (dirt :kurt 3)) 7  32)
;;(sample-trigger #(spacy (dirt :kurt 2)) 15 32)
;;(sample-trigger #(spacy (dirt :kurt 4)) 17 32)
;;(sample-trigger #(spacy (dirt :kurt 5)) 1 32)
;;(sample-trigger #(spacy (dirt :kurt 6)) 9 32)
;;(sample-trigger #(spacy (dirt :kurt 7)) 19 32)

;;(mono-player (dirt :pad 2))

(sample-trigger #(do
;;                   (echoey-buf boom-s :amp 0.1)
                   (echoey-buf (dirt :kurt 6) :amp 0.3)
;;                   (echoey-buf :b (dirt :kurt 1))
                   ) 31 32)

;;(remove-all-sample-triggers)

;;(spacy (dirt :cosmicg 2) :amp 0.5)
;;(on-beat-trigger 8 #(spacy (dirt :voodoo 0)))
;;(on-beat-trigger 64 #(echoey-buf (dirt :wind)))
;;

(doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 200 :amp 0.0 :attack 0.1 :noise-level 0.05 :release 1.0 :beat-trg-bus (:beat time/beat-2th) :wave 4 :beat-bus (:count time/beat-2th))
       (n-overtime! chord-g :amp 0 0.04))

(comment
  (ctl apeg-bass-wallop :saw-cutoff 10)
  (ctl apeg-deep-fast :saw-cutoff 10)
  (remove-all-beat-triggers)
  (stop)
  (kill wobbling)

 (fadeout-master)
 (recording-start "~/Desktop/flatiron10.wav")
 (recording-stop)
 )

(do (ctl drum-effects-g :amp 0.0) (ctl drums-g :amp 0.0))

(do
  (map #(set-beat % time/beat-1th) apeg-deep-melody)
  (map #(set-beat % time/beat-2th) slow-deep-chord-group)

  (let [chord-pat pinger-score
        chord-bufs [w-note3-b w-note8-b w-note9-b w-note10-b]]
    (dotimes [chord-idx (count chord-bufs)]
      (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))

  (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
        _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
        _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])

        chord-pat chords-score]
    (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b]]
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))
  )


(pattern! hats-buf [0])
(pattern! hats-buf [1])
(pattern! hats-buf [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])

(do (ctl drum-effects-g :amp 1.0) (ctl drums-g :amp 1.0))

(do
  (map #(set-beat % time/beat-2th) apeg-deep-melody)
  (map #(set-beat % time/beat-1th) slow-deep-chord-group)

  (let [chord-pat pinger-score
        chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b]]
    (dotimes [chord-idx (count chord-bufs)]
      (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))

  (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
        _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
        _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])

        chord-pat chords-score]
    (let [chord-bufs [w-note3-b w-note8-b w-note9-b w-note10-b]]
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))
  )
