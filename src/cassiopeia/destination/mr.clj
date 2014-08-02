(ns cassiopeia.destination.mr
  (:use overtone.live)
  (:use mud.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  (:use cassiopeia.dirt)
  (:require [mud.timing :as time]
            [clojure.math.numeric-tower :as math]
            [overtone.studio.fx :as fx]))

(ctl time/root-s :rate 8.)

(do
(defonce w-note6-b (buffer 256)) (defonce note1-dur-b (buffer 256))) (defonce w-note8-b (buffer 256)) (defonce w-note9-b (buffer 256)) (defonce w-note10-b (buffer 256)) (defonce w-note-b (buffer 256)) (defonce w-note2-b (buffer 256)) (defonce w-note3-b (buffer 256)) (defonce w-note7-b (buffer 256)) (defonce w-note5-b (buffer 256)) (defonce s-note-b (buffer 256))

(do
  (definst dulcet-fizzle [amp 0 c 200 n-rate 0.001 note-b 0 beat-trg-bus (:beat time/beat-8th) beat-bus (:count time/beat-8th)]
    (let [trg (in:kr beat-trg-bus)
          cnt (in:kr beat-bus)
          note (buf-rd:kr 1 note-b cnt)
          freq (midicps note)
          b (ringz (* 0.001 (rhpf (blip freq 3) (* 1.1 freq))) freq)
          n (ringz (* 0.001 (lpf (pink-noise) 200)) freq)
          s (* 0.001 (lpf (rhpf (saw (/ freq 2)) (* 2 freq)) (/ freq 2)))
          src (mix [n s])]
      (* amp (g-verb src))))
  (kill dulcet-fizzle)

  (defonce df-b (buffer 256))

  (def f (dulcet-fizzle :amp 1.0 :note-b df-b))
  ;;(ctl f :note (note :F4))
  )

(definst crackle-snail [notes-buf s-note-b
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
        src (mix [wave (lpf noize noise-cutoff)])
        src (g-verb src 200 1 0.2)
        e (env-gen (perc attack release) :gate gate-trg)
        amp (+ (* amp 5) amp)]
    (* amp e src)))

;;START

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
               )]
          (let [chord-bufs (shuffle [note1-b note2-b note3-b note4-b])] ;; Play around with some random inversions
            (dotimes [chord-idx (count chord-bufs)]
              (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))))
    ))

(defonce ws-note1-b (buffer 256))
(defonce ws-note2-b (buffer 256))
(defonce ws-note3-b (buffer 256))
(defonce ws-note4-b (buffer 256))

(defonce ws-note11-b (buffer 256))
(defonce ws-note12-b (buffer 256))
(defonce ws-note13-b (buffer 256))

(def apeg-deep-melody-spair
  [(deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note1-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note2-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note3-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note4-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)])

(def apeg-deep-melody
  [(deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note3-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note8-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note9-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf w-note10-b :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1)])

(def main-melody
  [(deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note11-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note12-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1)
   (deep-basz :amp 0.0 :noise-level 0.05 :notes-buf ws-note13-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1)])

(comment
  (map #(ctl %1 :saw-cutoff 800) slow-deep-chord-group)
  (map #(ctl %1 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)
  (map #(ctl %1 :saw-cutoff 800 :release 6 :noise 100.2 :attack 0.4 :amp 0.05) slow-deep-chord-group)
  (doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 1000 :amp 0.03 :attack 0.1 :noise-level 0.05 :release 1.0 :beat-trg-bus (:beat time/beat-2th) :wave 2 :beat-bus (:count time/beat-2th)))
)

(def slow-deep-chord-group
  (do
    ;;(kill deep-basz-pi)
    (defonce sd-g (group "slow deep chords"))
    (defonce sd-note1-b (buffer 256))
    (defonce sd-note2-b (buffer 256))
    (defonce sd-note3-b (buffer 256))
    (defonce sd-note4-b (buffer 256))
    (defonce sd-note5-b (buffer 256))
    (defonce sd-note6-b (buffer 256))
    (defonce sd-attack-b (buffer 256))
    (defonce sd-release-b (buffer 256))
    (defonce sd-amp-b (buffer 256))

    [(deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
  ;;   (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note5-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
;;     (deep-basz-pi [:head sd-g] :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.2 :noise-level 0.05 :notes-buf sd-note6-b :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th))
     ]))

(map #(map find-note-name %1) (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii]))
(map #(map find-note-name %) (chords-with-inversion [1 2] :F2 :minor :up 4))
(map #(map find-note-name %) (chords-with-inversion [1] :F2 :minor :up 3))


  )

(def apeg-swell
  (let [- [nil nil nil]
        chord-pat [(degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3)

                   (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3) (degrees [1] :minor :F3)

                   (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)

                   (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)
                   (degrees [5] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3) (degrees [5] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3) (degrees [4] :minor :F3)
                   ]
        chord-bufs [ws-note11-b ws-note12-b ws-note3-b ws-note13-b]]
    (dotimes [chord-idx (count chord-bufs)]
      (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))))

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

        f2iisus4 (chord :F2 :sus4 2)
        f2isus4 (chord :F2 :sus4 1)
        f2isus2 (chord :F2 :sus2 1)
        f2iisus2 (chord :F2 :sus2 2)

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
;;           fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
;;           fu23 fu23 fu23 fu23 fu23 fu23 fu23 fu23
;;           fu24 fu24 fu24 fu24 fu24 fu24 fu24 fu24

           fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  f26 f26 f26 f26 f26 f26 f26 f26
           fu23 fu23 fu23 fu23 fu23 fu23 fu23 fu23
           fu24 fu24 fu24 fu24 fu24 fu24 (chord :F2 :sus4 2) (chord :F2 :sus4 2)

           fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21  fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21 fuu21
           ;;f26 f26 f26 f26 f26 f26 f26 f26  f26 f26 f26 f26 f26 f26 f26 f26
           fu23 fu23 fu23 fu23 fu23 fu23 fu23 fu23
           fu25 fu25 fu25 fu25 fu25 fu25  (chord :F2 :7sus4 2) (chord :F2 :7sus4 2)
           ]]
      (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b]]
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))

(do
  (crackle-snail :noise-level 0.1 :amp 0.6)
  (pattern! s-note-b
            [(degrees [1] :minor :F1) (degrees [3] :minor :F1) 0 (degrees [4] :minor :F1) (degrees [1] :minor :F1) 0 0 0]
            (repeat 24 [0])))

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf bass-notes2-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf effects3-seq-buf]))

(do
  (pattern! effects2-seq-buf [1 1 0 0 0 0 0 0])
  (pattern! effects2-seq-buf [1 1 1  1 0 0  0 1 0  0 0 0  0 0 0])
  (pattern! effects2-seq-buf [1 0 0  1 1 1  0 0 0  1 0 0])
  (pattern! effects-seq-buf  (repeat 12 1)  [1 0 0 0])
  (pattern! effects2-seq-buf (repeat 12 0) [1 0 0 0] (repeat 16 0) (repeat 16 0))

  (kill seqer)

  (def clap2-drums (doall (map #(seqer [:head drum-effects-g]
                                       :rate-start 0.5 :rate-limit 0.6
                                       :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap2-s)) (range 0 16))))

  (def clap-drums  (doall (map #(seqer [:head drum-effects-g]
                                       :rate-start 0.5 :rate-limit 0.6
                                       :beat-num %1 :pattern effects2-seq-buf :amp 0.05 :num-steps 8 :buf (buffer-mix-to-mono clap-s)) (range 0 16)))))

(kill drum-effects-g)
(kill drums-g)

(pattern! bass-notes-buf
          (repeat 7 [(degrees [1] :minor :F2) 0 0 0])
          (repeat 7 [(degrees [2] :minor :F2) 0 0 0])
          [0 0 0 0]
          [0 (degrees [1] :minor :F2) (degrees [3] :minor :F2) (degrees [4] :minor :F2)])

(one-time-beat-trigger
 0 16
 (fn []
   (do
     (pattern! hats-buf      [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])
     (pattern! kick-seq-buf  [1 0 0 1 0 0 0 0   1 0 0 0 0 0 0 0])

     (def white (doall (map #(whitenoise-hat [:head drums-g] :amp 1.0 :seq-buf hats-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 24 :release 0.1 :attack 0.0 :beat-num %1) (range 0 24))))
     (ctl white :attack 0.002 :release 0.04 :amp 2)

     (def kicker (doall (map #(space-kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 32 :beat-num %1 :noise 0.05 :amp 4.2 :mod-index 0.1 :mod-freq 4.0 :mode-freq 0.2) (range 0 32))))
     (ctl kicker :attack 0.0 :sustain 0.2 :amp 1.0)
     )))

(one-time-beat-trigger
 0 16
 (fn []
   (doseq [s apeg-deep-melody]
     (ctl s :amp 0.00 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0)
     (n-overtime! s :amp 0 0.019 0.001))
   ))

(doseq [chord-g slow-deep-chord-group]
  (ctl chord-g :saw-cutoff 300 :amp 0.0 :attack 0.1 :noise-level 0.05 :release 1.0 :wave 4)
  (n-overtime! chord-g :amp 0 0.04 0.001))

(def hand-drums (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.3 :num-steps 16 :buf hand-drum-s :rate-start 1.0 :rate-limit 1.0) (range 0 16))))

(map #(ctl %1 :saw-cutoff 1000 :noise-level 0.5 :amp 0.09 :attack 0.3 :release 6.0 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)
(map #(ctl %1 :t 0.005 :amp 0.3) grumble-chord-group)
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

;;(ctl apeg-deep-melody :amp 0.05 :saw-cutoff 1500 :wave 3 :attack 0.01 :release 1.0)
(map #(ctl % :wave 0 :amp 0.1) apeg-deep-melody-spair)
(map #(ctl % :saw-cutoff 2500 :wave 4) apeg-deep-melody)
(map #(do (ctl % :amp 0.00 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0)
          (n-overtime! % :amp 0 0.019)) apeg-deep-melody)

(ctl apeg-deep-melody :amp 0.06 :saw-cutoff 1000 :wave 1 :attack 1.0 :release 5.0)
(set-beat apeg-deep-melody time/beat-1th)

(pattern! w-note8-b
          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]
          [0 (degrees [1] :minor :F3) (degrees [1] :minor :F4) 0]

          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
          [0 (degrees [1] :minor :F3) (degrees [6] :minor :F3) 0]
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

(def pinger-score-spair
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27]        (chords-for :C2 :minor 1)
        [c31 c32 c33 c34 c35 c36 c37]        (chords-for :C3 :minor 1)
        [f21 f22 f23 f24 f25 f26 f27]        (chords-for :F2 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37]        (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47]        (chords-for :F4 :minor 1)]
    (let [chord-pat
          [f41 f43 f41 f44 c37 c36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])]]
      (let [chord-bufs [ws-note1-b ws-note2-b ws-note3-b ws-note4-b]]
        (dotimes [chord-idx (count chord-bufs)]
                    (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))


;;(ctl (foundation-output-group) :master-volume 4)

;;(on-beat-trigger 256 #(echoey-buf pulse-s :amp 0.02))
;;(on-beat-trigger 64 #(echoey-buf godzilla-s :amp 0.3))
;;(on-beat-trigger 64 #(spacy constant-blues-s :amp 0.5))

;;(on-beat-trigger 32 #(spacy (dirt :kurt 1)))
;;(on-beat-trigger 64 #(spacy (dirt :kurt 3)))
;;(on-beat-trigger 128 #(spacy (dirt :kurt 2)))

;;(sample-trigger #(spacy (dirt :kurt 3)) 7  32)
;;(sample-trigger #(spacy (dirt :kurt 2)) 15 32)
;;;;(sample-trigger #(spacy (dirt :kurt 4)) 17 32)
;;(sample-trigger #(spacy (dirt :kurt 5)) 1 32)
;;(sample-trigger #(spacy (dirt :kurt 6)) 9 32)
;;
;;(remove-all-beat-triggers)
;;(remove-all-sample-triggers)

;;(mono-player (dirt :pad 2) :amp 0.2)

(sample-trigger #(do
;;                   (echoey-buf boom-s :amp 0.1)
                   (echoey-buf (dirt :kurt 6) :amp 0.1)
;;                   (echoey-buf :b (dirt :kurt 1))
                   ) 31 32)

;;(spacy (dirt :cosmicg 2) :amp 0.5)
;;(on-beat-trigger 8 #(spacy (dirt :voodoo 0)))
;;(on-beat-trigger 64 #(echoey-buf (dirt :wind) :amp 0.1))

(doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 300 :amp 0.00 :attack 0.1 :noise-level 0.05 :release 1.0 :beat-trg-bus (:beat time/beat-2th) :wave 4 :beat-bus (:count time/beat-2th))
       (n-overtime! chord-g :amp 0 0.04)
       )

(do ;;shh drums
  (ctl drum-effects-g :amp 0.0)
  (ctl drums-g :amp 0.0))

(spacy (dirt :pad 0) :amp 1.0)
(pattern! hats-buf [1])

(pattern! df-b [(degrees [1 1 1 1] :minor :F3)
                (degrees [1 1 1 1] :minor :F3)
                (degrees [1 1 1 1] :minor :F3)
;;                (degrees [6 6 6 6] :minor :F3)
        ;;        (degrees [7 7 7 7] :minor :F3)
                (degrees [1 1 1 1] :minor :F4)
                ])

(one-time-beat-trigger 126 128 #(plain-space-organ :tone (/ (midi->hz (note :F1)) 2) :duration 3 :amp 0.25))

(one-time-beat-trigger
 126 128
 (fn [] ;;DARKER PROGRESSION
   (do
     (doseq [s apeg-deep-melody] (ctl s :amp 0.00))

     (plain-space-organ :tone (/ (midi->hz (note :F1)) 2) :duration 3 :amp 0.5)
     (ctl drum-effects-g :amp 0.0)
     (ctl drums-g :amp 0.0)

     (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b sd-note5-b sd-note6-b]]
       (dotimes [chord-idx (count chord-bufs)]
         (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) dark-chords-score))))

     (let [chord-bufs [w-note3-b w-note8-b w-note9-b w-note10-b]]
       (dotimes [chord-idx (count chord-bufs)]
         (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) darker-pinger-score))))
     )
   (doseq [s apeg-deep-melody] (ctl s :amp 0.00 :saw-cutoff 100 :wave 0 :attack 1.0 :release 5.0)
          (n-overtime! s :saw-cutoff 100 2000 50)
          (n-overtime! s :amp 0.00 0.04 0.005))
;;   (doseq [s apeg-deep-melody] (ctl s :amp 0.00 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0) (n-overtime! s :amp 0.00 0.04 0.005))
   ))

;;Drive home home chords + highlight melody
(doseq [s main-melody] (ctl s :amp 0.09 :saw-cutoff 250 :wave 1 :attack 1.0 :release 5.0))

;;Drum tension
(pattern! hats-buf [1])
(map #(ctl % :amp 1.0) white)
(pattern! kick-seq-buf [1 0 0 0 1 0 0 0])
(map #(ctl % :amp 1.0) kicker)

(do
  (doseq [s main-melody] (ctl s :amp 0.0))
  (doseq [s apeg-deep-melody-spair]
    (ctl s :amp 0.00 :saw-cutoff 2000 :wave 2 :attack 1.0 :release 5.0)
    (n-overtime! s :amp 0.0 0.04 0.01)
    )
  (ctl drum-effects-g :amp 1.0) (ctl drums-g :amp 1.0)
  (doseq [s apeg-deep-melody] (ctl s :amp 0.05 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0))
  (def f (dulcet-fizzle :amp 2.0 :note-b df-b))

  ;;  (pattern! hats-buf [1 0 0 0 0 0 0 0])
  )

;;(ctl f :amp 2.4)
;;(doseq [s apeg-deep-melody] (ctl s :amp 0.05 :saw-cutoff 2500 :wave 0 :attack 1.0 :release 5.0))
;;(pattern! hats-buf [1])

(do
  (doall (map #(set-beat % time/beat-1th) apeg-deep-melody))
  (doall (map #(set-beat % time/beat-1th) apeg-deep-melody-spair))
  (doall (map #(set-beat % time/beat-2th) slow-deep-chord-group))

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
  (doall (map #(ctl % :amp 0) apeg-deep-melody-spair))
  (doall (map #(set-beat % time/beat-2th) apeg-deep-melody-spair))
  (doall (map #(set-beat % time/beat-2th) apeg-deep-melody))
  (doall (map #(set-beat % time/beat-1th) slow-deep-chord-group))

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

;;Experiments with new melody progression

(def dark-chords-score
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27]        (chords-for :C2 :minor 3)
        [f217 f227 f237 f247 f257 f267 f277] (chords-for :F2 :melodic-minor-asc 4)
        [fm31 fm32 fm33 fm34 fm35 fm36 fm37] (chords-for :F3 :major 4)

        [f31 f32 f33 f34 f35 f36 f37]        (chords-for :F3 :minor 3)
        [f317 f327 f337 f347 f357 f367 f377] (chords-for :F3 :minor 4)

        [f417 f427 f437 f447 f457 f467 f477] (chords-for :F4 :minor 4)

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

        f31dim (chord :F3 :dim)
        f42dim (chord :G2 :7sus4)
        f43dim (chord :A3 :dim)
        f44dim (chord :F3 :7sus4)
        f45dim (chord :C3 :7sus4)
        f46dim (chord :D3 :7sus4)
        f47dim (chord :E3 :7sus4)

        f3sus4   (chord :F3 :sus4)
        f3susi4  (chord :F3 :sus4 1)
        f3susii4 (chord :F3 :sus4 2)

        f4sus4   (chord :F4 :sus4)
        f4susi4  (chord :F4 :sus4 1)
        f4susii4 (chord :F4 :sus4 2)

        [fma21 fma22 fma23 fma24 fma25 fma26 fma27] (chords-with-inversion [] :F2 :melodic-minor-asc :up 3)
        [fmd21 fmd22 fmd23 fmd24 fmd25 fmd26 fmd27] (chords-with-inversion [] :F2 :melodic-minor-desc :up 3)

        all (chord-degree :ii :F3 :melodic-minor-asc)]
    (let [_ (pattern! sd-attack-b  [0.2];;(repeat 2 [0.06 0.32 0.32 0.32 0.32 0.32 0.32 0.32]) (repeat 2 [0.06 0.32 0.32 0.32])
                      )
          _ (pattern! sd-release-b [1.0];; [1.0  1.0 1.0 1.0]
                      )
          _ (pattern! sd-amp-b     [1];;  (repeat 2 [1.1  0.9 0.9 0.9 0.9 0.9 0.9 0.9]) (repeat 2 [1.1 0.9 0.9 0.9])
                      )
          chord-pat
          [
           ;; f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31
           ;; f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33
           ;; f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35
           ;; f357 f357 f357 f357 f357 f357 f357 f357 f337 f337 f337 f337 f337 f337 f337 f337

           ;; f31 f31 f31 f31  f31 f31 f31 f31 f31 f31 f31 f31  f31 f31 f31 f31
           ;; f33 f33 f33 f33  f33 f33 f33 f33  f33 f33 f33 f33 f33 f33 f33 f33
           ;; f35 f35 f35 f35  f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35
           ;; f357 f357 f357 f357 f357 f357 f357 f357 f367 f367 f367 f367 f367 f367 f367 f367
           ;; f31 f31 f31 f31 ;;f31 f31  f31 f31 f31 f31 f31 f31 f31 f31  f31 f31 f31 f31
           ;; f33 f33 f33 f33 ;;f33 f33  f33 f33 f33 f33  f33 f33 f33 f33 f33 f33 f33 f33
           ;; f34 f34 f34 f34 ;;f35 f35  f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35 f35
           ;; f3sus4 f3sus4 ;; f357 f357 f357 f357 f357 f357 f367 f367 f367 f367 f367 f367 f367 f367

           ;;(chord :F3 :5 1) (chord :F3 :5 1)
           ;;(chord :F3 :m+5) (chord :F3 :m+5)

           ;;(chord :F3 :m7+5 3) (chord :F3 :m7+5 3)
           ;;(chord :F4 :m7+5) (chord :F4 :m7+5)

           f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31
           f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33
           f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34
           f36 f36 f36 f36 f36 f36 f36 f36
           (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5) (chord :F3 :m+5)

           f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31 f31
           f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33 f33
           f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34 f34
           f36 f36 f36 f36 f36 f36 f36 f36
           (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5) (chord :F3 :m7+5)
           ]]
      (let [chord-bufs [sd-note1-b sd-note2-b sd-note3-b sd-note4-b sd-note5-b sd-note6-b]]
        (dotimes [chord-idx (count chord-bufs)]
          (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))

(def darker-pinger-score
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27]  (chords-for :C4 :minor 1)
        [c31 c32 c33 c34 c35 c36 c37]  (chords-for :C4 :minor 1)
        [f21 f22 f23 f24 f25 f26 f27]  (chords-for :F3 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37]  (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47]  (chords-for :F4 :minor 1)]
    (let [chord-pat
          [
           c31 f21 f23 f34    f21 f21 f41 f21
           ;;c31 f21 f23 f35    f31 f31 f41 f21
           ;;c31 f21 f23 f35    f31 f31 f41 f31
           ]
          ]
      (let [chord-bufs [w-note3-b w-note8-b w-note9-b w-note10-b]]
        (dotimes [chord-idx (count chord-bufs)]
                    (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat)))) chord-pat)))


(map #(ctl % :saw-cutoff 30 :amp 0.1) apeg-deep-melody)

;;Fade
(let [cutout 2000]
  (doall (map #(ctl % :saw-cutoff cutout :amp 0.03) apeg-deep-melody-spair))
  (doall (map #(ctl % :saw-cutoff cutout :amp 0.03) apeg-deep-melody))
  (doall (map #(ctl % :saw-cutoff cutout :amp 0.03) slow-deep-chord-group)))

(comment
  ;;(volume 4.0)
  ;;(ctl (foundation-output-group) :master-volume 1)
  (ctl drums-g :amp 0)
  (ctl drum-effects-g :amp 0)

  (remove-all-beat-triggers)
  (remove-all-sample-triggers)
  (stop)
  (kill wobbling)

  (fadeout-master)
  (recording-start "~/Desktop/flatiron14.wav")
  (recording-stop)
  )
