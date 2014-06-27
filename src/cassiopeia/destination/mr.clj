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

(def grumble-chord-group
  (do
    (defonce note1-b (buffer 256))
    (defonce note2-b (buffer 256))
    (defonce note3-b (buffer 256))
    (defonce note4-b (buffer 256))

    (defonce grumblers-g (group "the grumblers"))

    (kill wobbling)
    [(wobbling [:head grumblers-g] :notes-buf note1-b :amp 0.5 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.5)
     (wobbling [:head grumblers-g] :notes-buf note2-b :amp 0.5 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.5)
     (wobbling [:head grumblers-g]  :notes-buf note3-b :amp 0.5 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.5)
     (wobbling [:head grumblers-g]  :notes-buf note4-b :amp 0.5 :dur-buf note1-dur-b :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :lag-time 0.0 :t 0.5)]))

(map #(ctl %1 :t 0.0) grumble-chord-group)

(defn grumble-chords []
  (let [_ [0 0 0]
        [F21 F22 F23 F24 F25 F26 F27] (map #(chord-degree %1 :F2 :minor	 4) [:i :ii :iii :iv :v :vi :vii])
        [F31 F32 F33 F34 F35 F36 F37] (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
        [F41 F42 F43 F44 F45 F46 F47] (map #(chord-degree %1 :F4 :minor 4) [:i :ii :iii :iv :v :vi :vii])

        [Fa31 Fa32 Fa33 Fa34 Fa35 Fa36 Fa37] (map #(chord-degree %1 :F3 :minor 4) [:i :ii :iii :iv :v :vi :vii])
        [Fa41 Fa42 Fa43 Fa44 Fa45 Fa46 Fa47] (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])

        [C41 C42 C43 C44 C45 C46 C47] (map #(chord-degree %1 :C4 :minor	3) [:i :ii :iii :iv :v :vi :vii])
        [C31 C32 C33 C34 C35 C36 C37] (map #(chord-degree %1 :C3 :minor	3) [:i :ii :iii :iv :v :vi :vii])]

    (pattern! note1-dur-b [12 12 12 1/2 12 12 12 1/2])
    (let [chord-pat
          (concat [F31 F31 F31 F31 F31  _ _ _]
                  [F31 F31 F31 F31 F31  _ _ _]
                  [F31 _   F31 F31 F31  _ _ _]
                  [F31 _   F31 F31 F31  _ _ _]

                  [F31 F31 F31 _ _ _ _ _]
                  [F31 F31 F31 F31 F31 _ _ _]
                  [F31 F31 F31 _ _ _ _ _]
                  [F31 _   F31 F31 F31 _ _ _])]
      (let [chord-bufs (shuffle [note1-b note2-b note3-b note4-b])] ;; Play around with some random inversions
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))
  )

(defonce note4-b (buffer 256))
(def grumble4 (wobbling [:head grumblers-g]  :notes-buf note4-b :amp 4.0 :dur-buf note1-dur-b
                        :beat-bus (:count time/beat-1th)
                        :beat-trg-bus (:beat time/beat-1th)))

(defonce w-note-b (buffer 256))
(defonce w-note2-b (buffer 256))
(definst deep-basz [amp 1
                    notes-buf 0
                    noise-level 0.05
                    beat-trg-bus (:beat time/beat-4th)
                    beat-bus     (:count time/beat-4th)
                    attack 0.4
                    release 0.9
                    saw-cutoff 300]
  (let [trg (in:kr beat-trg-bus)
        cnt (in:kr beat-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        gate-trg (and (> note 0) trg)
        freq (midicps note)
        noize (* noise-level (pink-noise))
        src (mix [(lpf (saw freq) saw-cutoff)
                  (lpf noize 100)])
        src (g-verb src 200 1 0.2)
        e (env-gen (perc attack release) :gate gate-trg)
        amp (+ (* amp 5) amp)
        ]
    (* amp e src)))

(kill deep-basz)

(defonce w-note3-b (buffer 256))

(def apeg-deep (deep-basz :amp 0.7 :noise-level 0.05
                          :notes-buf w-note3-b
                          :beat-trg-bus (:beat time/beat-1th)
                          :beat-bus (:count time/beat-1th)
                          :attack 0.1
                          :release 0.1))

(do
  (kill deep-basz)
  (def slow-deep (deep-basz :amp 0.7
                            :noise-level 0.05
                            :notes-buf w-note-b
                            :beat-trg-bus (:beat time/beat-4th)
                            :beat-bus (:count time/beat-4th)
                            :saw-cutoff 0
                            :attack 0.4
                            ))

  ;;(ctl slow-deep :saw-cutoff 200)
  ;;(ctl highlight-deep :saw-cutoff 100)

  (def highlight-deep (deep-basz :amp 0.7 :noise-level 0.05
                                 :notes-buf w-note2-b
                                 :beat-trg-bus (:beat time/beat-2th)
                                 :beat-bus (:count time/beat-2th))))

;;(ctl sd-g :release 0.0 :attack 0.0 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :amp 0)
;;(ctl apeg-deep :saw-cutoff 500)

;;(ctl time/root-s :rate 0)
;;(kill drums-g)

;;(map #(ctl %1 :saw-cutoff 1000 :amp 0.05 :attack 0.8 :release 0.8 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)

;;(do(doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 2000 :amp 0.2 :attack 0.1 :release 1.0 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)))

  ;;  (ctl time/root-s :rate 8)

  ;;  (def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 2.2 :mod-index 0.1 :mod-freq 10.2)))
  ;;)

;;(ctl drums-g :mod-index 0.0 :amp 2.2 :mod-freq 0)


(def slow-deep-chord-group
  (do
    (kill deep-basz)
    (defonce sd-g (group "slow deep chords"))
    (defonce sd-note1-b (buffer 256))
    (defonce sd-note2-b (buffer 256))
    (defonce sd-note3-b (buffer 256))
    (defonce sd-note4-b (buffer 256))
    [(deep-basz [:head sd-g] :attack 1 :release 8.0 :amp 0.4 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
     (deep-basz [:head sd-g] :attack 1 :release 8.0 :amp 0.4 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
     (deep-basz [:head sd-g] :attack 1 :release 8.0 :amp 0.4 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
     (deep-basz [:head sd-g] :attack 1 :release 8.0 :amp 0.4 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))]))

(map #(map find-note-name %1) (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii]))

(let [_ [0 0 0]
      [f21 f22 f23 f24 f25 f26 f27]        (map #(chord-degree %1 :F2 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [fm21 fm22 fm23 fm24 fm25 fm26 fm27] (map #(chord-degree %1 :F3 :minor 4) [:i :ii :iii :iv :v :vi :vii])
      [f31 f32 f33 f34 f35 f36 f37]        (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f41 f42 f43 f44 f45 f46 f47]        (map #(chord-degree %1 :F4 :major 4) [:i :ii :iii :iv :v :vi :vii])]
  (let [chord-pat
        [f31 f31 f31 f31;; f31 f31 f31 f31
         f33 f33 f32 f34 ;;f33 f33
         f34 f34 f32 f32 ;;f34 f34

         ;;fm21 fm21 fm21 fm21
         ;;fm23 fm23 fm23 fm23
         ;;fm24 fm24 fm24 fm24

         ;;f34 f34 f35 f35 f35 f35 f31 f31

         ]]
    (let [chord-bufs (shuffle [sd-note1-b sd-note2-b sd-note3-b sd-note4-b])] ;; Play around with some random inversions
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))

(defonce d-n1 (buffer 256))
(defonce d-n2 (buffer 256))
(defonce d-n3 (buffer 256))

(let [_ [0 0 0]
      [f21 f22 f23 f24 f25 f26 f27] (map #(chord-degree %1 :F2 :major 3) [:i :ii :iii :iv :v :vi :vii])
      [f31 f32 f33 f34 f35 f36 f37] (map #(chord-degree %1 :F3 :major 3) [:i :ii :iii :iv :v :vi :vii])
      [f41 f42 f43 f44 f45 f46 f47] (map #(chord-degree %1 :F4 :major 3) [:i :ii :iii :iv :v :vi :vii])]

  (let [chord-pat
        [f31 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
         f33 _ _ _ _ _ _ _
         f35 _ _ _ _ _ _ _
         f37 _ _ _ _ _ _ _]]
    (let [chord-bufs (shuffle [d-n1 d-n2 d-n3])] ;; Play around with some random inversions
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))

(do

  (deep-basz :amp 0.7 :noise-level 0.05
             :notes-buf d-n3
             :beat-trg-bus (:beat time/beat-1th)
             :beat-bus (:count time/beat-1th)
             :attack 0.1
             :release 2.1
             )

  (deep-basz :amp 0.7 :noise-level 0.05
             :notes-buf d-n2
             :beat-trg-bus (:beat time/beat-1th)
             :beat-bus (:count time/beat-1th)
             :attack 0.1
             :release 2.1)

  (deep-basz :amp 0.7 :noise-level 0.05
             :notes-buf d-n1
             :beat-trg-bus (:beat time/beat-1th)
             :beat-bus (:count time/beat-1th)
             :attack 0.1
             :release 2.1
             )
  )

(pattern! w-note3-b
          (repeat 8 [(degrees [1 3 5 4] :minor :F3)])
          (repeat 5 [(degrees [1] :minor :F3) 0 (degrees [3] :minor :F3) 0])
          (repeat 3 [(degrees [4] :minor :F3) 0 (degrees [1] :minor :F3) 0])

          (repeat 8 [(degrees [6 1 3 5] :minor :F3)])
          (repeat 5 [(degrees [5] :minor :F3) 0 (degrees [3] :minor :F3) 0])
          (repeat 3 [(degrees [4] :minor :F3) 0 (degrees [1] :minor :F3) 0])

          (repeat 8 [(degrees [6 4 2 1] :minor :F3)])
          (repeat 4 [(degrees [1] :minor :F3) 0 (degrees [3] :minor :F3) 0])
          (repeat 4 [(degrees [4] :minor :F3) 0 (degrees [1] :minor :F3) 0])

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
          (repeat 8 [(degrees [1] :major :F3) (degrees [3] :major :F3) (degrees [4] :major :F3) ])
          (repeat 4 [(degrees [5] :major :F3) 0 (degrees [3] :major :F3) 0])
          (repeat 4 [(degrees [4] :major :F3) 0 (degrees [1] :major :F3) 0])

          (repeat 8 [(degrees [3] :major :F3) (degrees [3] :major :F3) (degrees [5] :major :F3) ])
          (repeat 4 [(degrees [5] :major :F3) 0 (degrees [3] :major :F3) 0])
          (repeat 4 [(degrees [4] :major :F3) 0 (degrees [1] :major :F3) 0])

          (repeat 8 [(degrees [1] :major :F3)   (degrees [3] :major :F3) (degrees [5] :major :F3)])
          (repeat 4 [(degrees [5] :major :F3) 0 (degrees [3] :major :F3) 0])
          (repeat 4 [(degrees [4] :major :F3) 0 (degrees [1] :major :F3) 0])
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
          src (lpf (mix [noize (pulse:ar freq 0.01)]) 2000)
          src (pitch-shift src 0.01 0.9 1 0.1)
          e (env-gen (adsr :attack 0.6 :release 2.0 :sustain 2.0) :gate gate-trg)]
      (* (+ (* amp 1) amp) e src)))

  (kill sawy)
  (sawy :noise-level 0.1 :amp 0.2)
  (pattern! s-note-b
            [(degrees [1] :minor :F1) (degrees [3] :minor :F1) 0 (degrees [4] :minor :F1) (degrees [1] :minor :F1) 0]
            (repeat 16 [0])
;;            (repeat (* 3 8) [0])
;;            (repeat (* 3 8) [0])
)
)

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
;;          src (pitch-shift src 0.01 0.9 1 0.1)
          e (env-gen (adsr :release 0.9 :sustain 0.9) :gate gate-trg)]
      (* (+ (* amp 1) amp) e src)))

  (kill fizzal)
  (fizzal :amp 0.3)
  (pattern! f-note-b
            (repeat (* 2 4) [(degrees [1] :minor :F2)]) (repeat (* 2 4) [0])
            (repeat (* 2 4) [(degrees [1] :minor :F2)]) (repeat (* 2 4) [0])))

(stop)
(on-beat-trigger 64 #(grumble-chords))
(remove-all-beat-triggers)


(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf bass-notes2-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

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

(def clap2-drums (doall (map #(seqer [:head drum-effects-g]
                                     :rate-start 0.5 :rate-limit 0.6
                                     :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))
(def clap-drums  (doall (map #(seqer [:head drum-effects-g]
                                     :rate-start 0.5 :rate-limit 0.6
                                     :beat-num %1 :pattern effects-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16))))

(kill drum-effects-g)

;;(note :E2)
(pattern! bass-notes-buf
          (repeat 16 [:F2])
          (repeat 16 [:G2]))

(pattern! bass-notes2-buf
          (repeat 16 [:F2])
          (repeat 16 [:G2]))

(kill drums-g)
(pattern! bass-notes-buf
          (repeat 7 [(degrees [1] :minor :F2) 0 0 0])
          (repeat 7 [(degrees [2] :minor :F2) 0 0 0])
          [0 0 0 0]
          [0 (degrees [1] :minor :F2) (degrees [3] :minor :F2) (degrees [4] :minor :F2)])

(pattern! kick-seq-buf (repeat 14 [1 0 0 0]) [0 0 0 0] [0 1 1 1])

(def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 2.2 :mod-index 0.1 :mod-freq 10.2)))
(ctl drums-g :mod-index 0.0 :amp 2.2 :mod-freq 0)

;;(ctl apeg-deep :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th) :attack 1 :release 10)
;;(ctl apeg-deep :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th ) :attack 0.1 :release 0.1)
;;(ctl drums-g :amp 2)

(pattern! kick-seq-buf [1 0 0 0 0 0 0 0])
(pattern! kick-seq-buf [1 0 0 0])
(pattern! kick-seq-buf [1 0 0])
(pattern! kick-seq-buf [1 0])
(pattern! kick-seq-buf [1])

(pattern! kick-seq-buf
          [1 0 0 0 1 0 0 0]
          [1 0 0 0 1 0 0 0]
          [1 0 0 0 1 0 0 0]
          [1 0 0 0 1 0 0 0]

          [1 0 0 0 0 0 0 0]
          [1 0 0 0 1 0 0 0]
          [1 0 0 0 0 0 0 0]
          [1 0 0 0 1 1 1 1])

(pattern! hats-buf
          [0 0 1 0 0 0 1 0]
          [0 0 1 0 0 0 1 0]
          [0 0 1 0 0 0 1 0]
          [0 0 1 0 0 0 1 0]

          [1 0 1 0 0 0 1 0]
          [0 0 1 0 0 0 1 0]
          [1 0 1 0 0 0 1 0]
          [0 0 1 0 0 0 1 0])

(pattern! bass-notes-buf
          (repeat 8 (degrees [1] :minor :F1))
          (repeat 2 (repeat 8 (degrees [1] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          [(degrees [1 1 1 1  5 4 3 1] :minor :F1)])

(pattern! effects-seq-buf
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]

          [1 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [1 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0])

(pattern! effects2-seq-buf
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]

          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0])

(pattern! effects-seq-buf
          [1 0 1 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]

          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [1 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0])

(def hats (doall (map #(high-hats [:head drums-g] :amp 0.3 :mix (nth (take 32 (cycle [1.0 1.0])) %1) :room 4 :note-buf bass-notes2-buf :seq-buf hats-buf :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.0 :mix 0.1 :room 1 :amp 0.5)

;;(ctl (foundation-output-group) :master-volume 1)
;;(stop)
(kill grumblers-g)

;;(on-beat-trigger 64 #(echoey-buf pulse-s :amp 0.1))
;;(on-beat-trigger 64 #(echoey-buf godzilla-s :amp 0.3))
;;(on-beat-trigger 64 #(spacy constant-blues-s :amp 0.5))

;;(on-beat-trigger 8 #(dirt :kurt 1))
;;(on-beat-trigger 16 #(dirt :kurt 0))

(remove-all-beat-triggers)

(stop)

(comment
 (fadeout-master)
 (recording-start "~/Desktop/dumdum2.wav")
 (recording-stop)
 )
