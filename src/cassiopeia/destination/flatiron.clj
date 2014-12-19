(ns cassiopeia.destination.flatiron
"
.-. .   .-. .-. .-. .-. .-. .  .
|-  |   |-|  |   |  |(  | | |\\|
'   `-' ` '  '  `-' ' ' `-' ' ``"
(:use [overtone.live] [mud.core] [mud.chords] [cassiopeia.waves.synths] [cassiopeia.samples] [cassiopeia.engine.buffers] [cassiopeia.dirt] [cassiopeia.waves.buf-effects] [cassiopeia.engine.expediency])
(:require [mud.timing :as time] [clojure.math.numeric-tower :as math] [overtone.studio.fx :as fx] [shadertone.tone :as t]))

(def master-vol 3.0)
(volume master-vol)
(ctl-global-clock 0.0)

(do
  (defbufs 256 [df-b sd-attack-b sd-release-b sd-amp-b s-note-b])
  (def apeg-deep-melody-spair-chord-g
    (chord-synth general-purpose-assembly 4 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1))
  (def apeg-deep-melody-chord-g
    (chord-synth general-purpose-assembly 4 :amp 0.00 :saw-cutoff 2000 :wave 0 :attack 1.0 :release 5.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th)))
  (def main-melody-chord-g
    (chord-synth general-purpose-assembly 3 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1))

  (defonce sd-g (group "slow deep chords"))
  (def slow-deep-chord-g
    ;;Needs 4
    (chord-synth general-purpose-assembly-pi 4 [:head sd-g] :saw-cutoff 300 :amp 0.0 :attack 0.1 :noise-level 0.05 :release 1.0 :wave 4 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.3 :release 6.0 :noise-level 0.05 :amp-buf sd-amp-b :release-buf sd-release-b :attack-buf sd-attack-b))
  (def apeg-start (first (:bufs apeg-deep-melody-chord-g)))
  )

(do
(def dark-chords-score
  (let [_ [0 0 0 0]
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 3)
        _ (pattern! sd-attack-b  [0.2])
        _ (pattern! sd-release-b [1.0])
        _ (pattern! sd-amp-b     [1])

        chord-pat (concat
                   (repeat 16 f31)
                   (repeat 16 f33)
                   (repeat 16 f34)
                   (repeat 8 f36) (repeat 8 (chord :F3 :m+5))

                   (repeat 16 f31)
                   (repeat 16 f33)
                   (repeat 16 f34)
                   (repeat 8 f36) (repeat 8 (chord :F3 :m7+5)))]
    chord-pat))

(def darker-pinger-score
  (let [_ [0 0 0 0]
        [c31 c32 c33 c34 c35 c36 c37]  (chords-for :C3 :minor 1)
        [c41 c42 c43 c44 c45 c46 c47]  (chords-for :C4 :minor 1)
        [f21 f22 f23 f24 f25 f26 f27]  (chords-for :F2 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37]  (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47]  (chords-for :F4 :minor 1)]
    (chord-score (repeat 6 [c41 f31 f33 f34  f31 f31 f41 f31  c41 f31 f33 f34  f31 f31 f41 f31])
                           [c37 f31 f33 f34  f31 f31 f41 f31  c37 f31 f33 f34  f31 f31 f41 f31]
                           [c41 f31 f33 f34  f31 f31 f41 f31  c34 f31 f33 f31  f31 f31 f41 f31]

                 (repeat 6 [c41 f31 f33 f34  f31 f31 f41 f31  c41 f31 f33 f34  f31 f31 f41 f31])
                           [c37 f31 f33 f34  f31 f31 f41 f31  c37 f31 f33 f34  f31 f31 f41 f31]
                           [c41 f31 f33 f41  f27 f31 f31 f31  c31 f31 f33 f41  f31 f31 f41 f31])))

(def apeg-swell
  (chord-score
   (repeat 16 (degrees [1] :minor :F3))
   (repeat 16 (degrees [1] :minor :F3))
   (repeat 16 (degrees [4] :minor :F3))
   (repeat 8 (degrees [4] :minor :F3))
   (repeat 4 (degrees [5] :minor :F3))
   (repeat 4 (degrees [4] :minor :F3))))

(def chords-score
  (let [_ [0 0 0 0]
        [fu21 fu22 fu23 fu24 fu25 fu26 fu27]          (chords-with-inversion [1] :F2 :minor :up 3)
        [fuu21 fuu22 fuu23 fuu24 fuu25 fuu26 fuu27]   (chords-with-inversion [1 2] :F2 :minor :up 3)
        [f21 f22 f23 f24 f25 f26 f27]                 (chords-for :F2 :minor 3)

        all (chord-degree :ii :F3 :melodic-minor-asc)]
    (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
          _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
          _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])
          chord-pat
          (concat
           (repeat 8 fuu21)
           (repeat 8 f26)
           (repeat 8 fu23)
           [fu24 fu24 fu24 fu24 fu24 fu24 (chord :F2 :sus4 2) (chord :F2 :sus4 2)]

           (repeat 8 fuu21)
           (repeat 8 fuu21)
           (repeat 8 fu23)
           [fu25 fu25 fu25 fu25 fu25 fu25  (chord :F2 :7sus4 2) (chord :F2 :7sus4 2)])]
      chord-pat)))

(def pinger-score-alternative
  (let [_ [0 0 0 0]
         [c21 c22 c23 c24 c25 c26 c27] (chords-for :C2 :minor 1)
         [c31 c32 c33 c34 c35 c36 c37] (chords-for :C3 :minor 1)
         [c41 c42 c43 c44 c45 c46 c47] (chords-for :C4 :minor 1)
         [c41 c42 c43 c44 c45 c46 c47] (chords-for :C4 :minor 1)
         [f21 f22 f23 f24 f25 f26 f27] (chords-for :F2 :minor 1)
         [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 1)
         [f41 f42 f43 f44 f45 f46 f47] (chords-for :F4 :minor 1)
         [f31i f32i f33i f34i f35i f36i f37i] (chords-with-inversion [1 2] :F3 :minor 1)]

    [f43 _   f43 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     c41 f35 f31 f34 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     c41 f35 f31 f34 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f41 _   f43 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f37 f41 _ f41 c35 _ (as-chord (degrees [1] :minor :F3)) (as-chord (degrees [1] :minor :F3))
     f37 f41 _ f41 c41 _ (as-chord (degrees [1] :minor :F3) ) (as-chord (degrees [1] :minor :F3))
     ;;--
     f41 _   f43 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f41 _   f43 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f41 _   f43 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 f36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f37 f41 _ f41 c35 _  (as-chord (degrees [1] :minor :F3)) (as-chord (degrees [1] :minor :F3))
     f37 f41 _ f41 c41 _  (as-chord (degrees [1] :minor :F3)) (as-chord (degrees [1] :minor :F3))]))

(def pinger-score-highlighted
  (let [_ [0 0 0 0]
        [c21 c22 c23 c24 c25 c26 c27] (chords-for :C2 :minor 1)
        [c31 c32 c33 c34 c35 c36 c37] (chords-for :C3 :minor 1)
        [c41 c42 c43 c44 c45 c46 c47] (chords-for :C4 :minor 1)
        [c41 c42 c43 c44 c45 c46 c47] (chords-for :C4 :minor 1)
        [f21 f22 f23 f24 f25 f26 f27] (chords-for :F2 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47] (chords-for :F4 :minor 1)
        [f413 f423 f433 f443 f453 f463 f473] (chords-for :F3 :minor 2)]

    [f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3) )
     f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3) )
     f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

     f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 c43 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 c44 c47 c46 (as-chord (degrees [3] :minor :C4)) (as-chord (degrees [5] :minor :F3))

     ;;-

     f41 f43 f41 f44 f37 c36 [(degrees [1] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 c36 [(degrees [1] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f41 c36 [(degrees [1] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f41 c36 [(degrees [1] :minor :F3)] (as-chord (degrees [7] :minor :F3))

     f41 f43 f41 f44 c43 c36 [(degrees [7] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 c43 c36 [(degrees [7] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 c41 c36 [(degrees [1] :minor :F3)] (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 c44 c47 c36 f453 f413]))

(chords-for :F4 :minor 1)
(degrees [:i] :minor :F4) 65 67 68 70 72 73 75
(degrees [:i] :minor :F4)

(pattern! (first (:bufs apeg-deep-melody-chord-g))
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1 3 1 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3)
          (degrees [1] :minor :F4) [0] (degrees [3 4] :minor :F4) (degrees [7 6] :minor :C3) (degrees [7 7] :minor :F3))

(pattern! (first (:bufs apeg-deep-melody-chord-g))  (degrees [1] :minor :F4))

(def pinger-score
  (let [_ [0 0 0 0]
        [c31 c32 c33 c34 c35 c36 c37] (chords-for :C3 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47] (chords-for :F4 :minor 1)]
    (let [new-pat (chord-score
                   (repeat 15 [f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))])
                   [f41 _   f43 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))])]

      [f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))

       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 f43 f41 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
       f41 _   f43 f44 c37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))])))

(def pinger-growth-score-spair
  (let [_ [0 0 0 0]
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47] (chords-for :F4 :minor 1)]
    [f41 _ f43 f44 f37 f36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])
     f41 f43 f41 f44 f37 f36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])
     f41 f43 f41 f44 f37 f36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])
     f41 f43 f41 f44 f37 f36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])]))

(def pinger-score-spair
  (let [_ [0 0 0 0]
        [c31 c32 c33 c34 c35 c36 c37] (chords-for :C3 :minor 1)
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 1)
        [f41 f42 f43 f44 f45 f46 f47] (chords-for :F4 :minor 1)]
    [f41 f43 f41 f44 c37 c36 (flatten [(degrees [7] :minor :F3) 0 0 0]) (flatten [(degrees [7] :minor :F3) 0 0 0])]))

(chord-pattern! main-melody-chord-g apeg-swell)
(chord-pattern! slow-deep-chord-g chords-score)
(chord-pattern! apeg-deep-melody-chord-g pinger-score)
(chord-pattern! apeg-deep-melody-spair-chord-g pinger-score-spair)

(defonce effects-seq-buf (buffer 256))
(pattern! effects-seq-buf  (repeat 12 1)  [1 0 0 0])
)

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf bass-notes2-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]) (defonce hats-amp (buffer 256)) (defonce kick-amp (buffer 256)))

(pattern! bass-notes-buf
          (repeat 8 (degrees [1] :minor :F1))
          (repeat 2 (repeat 8 (degrees [1] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
          [(degrees [1 1 1 1  5 4 3 1] :minor :F1)])

(pattern! kick-amp  [1.5 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1] (repeat 2 [1.2 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1]) [1.2 1 1 1 1 1 1 1   1.2 1 1 1 1.2 1 1.3 1])
(pattern! hats-amp  (repeat 3 [2 2 2 2 2.1 2 2 2   2 2 2 2 2 2 2 2]) [2 2 2 2 2.1 2 2 2   2 2 2.4 2 2.4 2 2 2])

(pattern! effects-seq-buf  (repeat 12 1)  [1 0 0 0])

(one-time-beat-trigger
 15 16
 (fn []
   (do
     (pattern! hats-buf (repeat 3 [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])
                                  [0 0 0 0 1 0 0 0   0 0 1 0 1 0 0 0])
     (pattern! kick-seq-buf
               (repeat 3 [1 0 0 1 0 0 0 0   1 0 0 0 0 0 0 0])
                         [1 0 0 1 0 0 0 0   1 0 0 0 1 0 1 0])

     (def white (doall (map #(whitenoise-hat [:head drums-g] :amp-buf hats-amp :seq-buf hats-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :release 0.1 :attack 0.0 :beat-num %1) (range 0 1))))
     (ctl white :amp-buf hats-amp)
     (ctl white :attack 0.04 :release 0.01 :amp 1)
     (ctl white :attack 0.002 :release 0.04 :amp 2)

     (def kicker (doall (map #(space-kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 16 :beat-num %1 :noise 0.05 :amp 4.2 :mod-index 0.1 :mod-freq 4.0 :mode-freq 0.2) (range 0 1))))
     (ctl kicker :amp-buf kick-amp :attack 0.0 :sustain 0.2 :amp 1.0)
     )))

;;START
(one-time-beat-trigger 0 16 #(n-overtime! apeg-deep-melody-chord-g :amp 0.0 0.019 0.0002))

;(grainy-buf :b (buffer-mix-to-mono rf-fx-s) :amp 0.3 :dur 5.0 :trate 1 :amp 0.9)
;;(echoey-buf rf-theorems-s :amp 0.58)

(do
  (ctl slow-deep-chord-g :wave 4)
  (n-overtime! slow-deep-chord-g :amp 0.0 0.04 0.0008))

(def hand-drums (efficient-seqer [:head drum-effects-g] :pattern effects-seq-buf :amp 0.25 :num-steps 16 :buf hand-drum-s :rate-start 0.9 :rate-limit 1.0))

(pattern! hats-buf
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 1 0 0 1 0]
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 1 0 0 1 0])

(pattern! hats-amp
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 1 0 0 1 0]
          [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1 0 0 0 1 0] [0 0 1.5 1.5 0 0 1 0])

;;(on-beat-trigger 256 #(echoey-buf pulse-s :amp 0.02))
;;(on-beat-trigger 128 #(echoey-buf godzilla-s :amp 0.4))
;;(on-beat-trigger 256 #(echoey-buf ooo-s :amp 0.1))
;;(spacy (dirt :cosmicg 2) :amp 0.1)

;;(sample-trigger 31 32 #(do (echoey-buf (dirt :kurt 6) :amp 0.23)))
;;(remove-all-sample-triggers)
;;(remove-all-beat-triggers)

(one-time-beat-trigger 126 128 (fn [& _]
                                 ;;(reset! cells-weight 2.0) (reset! invert-color 1.0) (remove-all-beat-triggers)(reset! cell-dance-weight 0.0)
                                 (chord-pattern apeg-deep-melody-chord-g pinger-score-highlighted)
                                 (plain-space-organ :tone (/ (midi->hz (note :F1)) 2) :duration 3 :amp 0.25)))
(one-time-beat-trigger
 126 128
 (fn [] ;;DARKER PROGRESSION
   (do
     ;;(reset! cells-weight 4.0)(reset! circular-weight 1.0)(reset! invert-color 1.0)

     (plain-space-organ :tone (/ (midi->hz (note :F1)) 2) :duration 3 :amp 0.45)
     (ctl apeg-deep-melody-chord-g :amp 0.00)
     (ctl drum-effects-g :amp 0.0)

     (ctl drums-g :amp 0.0)

     (chord-pattern slow-deep-chord-g dark-chords-score )
     (chord-pattern apeg-deep-melody-chord-g darker-pinger-score)
     )
   (doseq [s (:synths apeg-deep-melody-chord-g)]
     (ctl s :amp 0.00 :saw-cutoff 100 :wave 0 :attack 1.0 :release 5.0)
     (n-overtime! s :saw-cutoff 100 2000 50)
     (n-overtime! s :amp 0.00 0.04 0.005))))


;;(on-beat-trigger 64 #(echoey-buf (dirt :wind) :amp 0.1))
;;Drive home home chords + highlight melody
(ctl main-melody-chord-g :amp 0.1 :saw-cutoff 50 :wave 1 :attack 1.0 :release 5.0)
(ctl apeg-deep-melody-chord-g :amp 0.038 :saw-cutoff 2800 :wave 1)

;;Drum tension
(pattern! kick-seq-buf [1 0 0 0 1 0 0 0])
(ctl kicker :amp 1.0)
(ctl white :amp 1.0)

(do
  ;;(reset! circle-destruction (* Math/PI 0.5)) (reset! invert-color 0.0)
  (ctl main-melody-chord-g :amp 0.0)
  (ctl apeg-deep-melody-spair-chord-g :amp 0.00 :saw-cutoff 2000 :wave 2 :attack 1.0 :release 5.0)
  (n-overtime! apeg-deep-melody-spair-chord-g :amp 0 0.04 0.01)

  (chord-pattern apeg-deep-melody-spair-chord-g pinger-growth-score-spair)
  (ctl drum-effects-g :amp 0.3) (ctl drums-g :amp 1.0)

  (pattern! effects-seq-buf  (repeat 12 [1 0])  [1 0 0 0])
  (ctl apeg-deep-melody-chord-g :amp 0.05 :saw-cutoff 2600 :wave 0 :attack 1.0 :release 5.0)
  (def f (dulcet-fizzle :amp 2.0 :note-buf df-b))
  )

(do
  ;;(on-beat-trigger 8 #(do (swap! circle-destruction + (rand 1.0))))

  (ctl apeg-deep-melody-spair-chord-g :amp 0)
  (ctl-time apeg-deep-melody-spair-chord-g time/beat-2th)
  (ctl-time apeg-deep-melody-chord-g time/beat-2th)
  (ctl-time slow-deep-chord-g time/beat-1th)

  (chord-pattern slow-deep-chord-g pinger-score)

  (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.1])
        _ (pattern! sd-release-b [1.0 0.6 0.4 0.2])
        _ (pattern! sd-amp-b     [1.2 0.9 0.9 0.8])]
    (chord-pattern apeg-deep-melody-chord-g chords-score)))

(do
  (pattern! kick-seq-buf
            (repeat 3 (concat [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 0 0]))
            [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 1 0])
  (def f (dulcet-fizzle :amp 2.0 :note-buf df-b)))

(ctl kicker :amp 1.5)
(ctl white  :amp 1.5)

(one-time-beat-trigger 126 128
                       (fn [& _]
                         (ctl-time apeg-deep-melody-chord-g time/beat-1th)
                         (ctl-time apeg-deep-melody-spair-chord-g time/beat-1th)
                         (ctl-time slow-deep-chord-g time/beat-2th)

                         (one-time-beat-trigger
                          127 128
                          (fn [& _]
                            (def apeg-deep-melody-spair2-chord-g
                              (chord-synth general-purpose-assembly 4 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1))

                            (chord-pattern apeg-deep-melody-spair2-chord-g pinger-score-alternative)

                            (ctl-time apeg-deep-melody-chord-g time/beat-1th)
                            (ctl-time apeg-deep-melody-spair-chord-g time/beat-1th)
                            (ctl-time slow-deep-chord-g time/beat-2th)

                            (ctl main-melody-chord-g :amp 0.03)
                            (ctl apeg-deep-melody-spair2-chord-g :amp 0.03)
                            (chord-pattern main-melody-chord-g pinger-score-spair)
                            (n-overtime! apeg-deep-melody-spair2-chord-g :saw-cutoff 0.0 1000 50)
                            (n-overtime! apeg-deep-melody-spair-chord-g  :saw-cutoff 0.0 2600 50)
                            (n-overtime! main-melody-chord-g             :saw-cutoff 0.0 1000 50)

                            (chord-pattern apeg-deep-melody-spair-chord-g  pinger-growth-score-spair)
                            (chord-pattern apeg-deep-melody-chord-g        pinger-score-highlighted)

                            (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
                                  _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
                                  _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])]
                              (chord-pattern slow-deep-chord-g chords-score))
                            ))))

;;More fizzle
;;(doall (map #(n-overtime! % :saw-cutoff 2600.0 0 50) (:synths apeg-deep-melody-chord-g)))
(on-beat-trigger 64 #(do (plain-space-organ :tone (/ (midi->hz (note :F2)) 2) :duration 3 :amp 0.2)))

(do
  (def main-melody2-chord-g (chord-synth general-purpose-assembly 3 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1))
  (chord-pattern main-melody2-chord-g apeg-swell)

  (reset! color 0.5)
  (chord-pattern main-melody2-chord-g  darker-pinger-score)
  (ctl main-melody2-chord-g :amp 0.03 :saw-cutoff 1000)
  (ctl main-melody-chord-g :saw-cutoff 300 :amp 0.03)
  (chord-pattern main-melody-chord-g apeg-swell))


;;Fade
(let [cutout 2000]
  (kill buf->perc-inst)
  (kill buf->smooth-inst)
  (remove-all-beat-triggers)
  (ctl drums-g :amp 0)
  (ctl drum-effects-g :amp 0)
  (ctl apeg-deep-melody-spair-chord-g :saw-cutoff cutout)
  (ctl apeg-deep-melody-spair2-chord-g :saw-cutoff)
  (ctl apeg-deep-melody-chord-g :saw-cutoff cutout)
  (ctl main-melody-chord-g :saw-cutoff cutout)
  (ctl main-melody2-chord-g :saw-cutoff cutout)
  (ctl slow-deep-chord-g :saw-cutoff cutout)
  )

;;(echoey-buf rf-full-s :amp 0.2 :decay 1.5 :delay 0.1)
;;(spacy rf-full-s :amp 0.6)
;;(echoey-buf rf-full-s :amp 0.04)

(comment
  (do
    (def voices 8)
    (def durations [1/8 1/4 1/2 1])
    (def pattern-size 8)
    (def perc-samples [rf-full-s rf-full-s rf-theorems-s rf-fx-s rf-theorems-s rf-theorems-s rf-full-s rf-full-s])
    (def smooth-samples [rf-fx-s rf-solve-s rf-theorems-s rf-full-s rf-solve-s rf-fx-s rf-full-s rf-solve-s])
    )

  (kill buf->perc-inst)
  (kill buf->smooth-inst)

  (on-beat-trigger 64 #(do (spin-for (rand-int voices) durations (:duration gs))))
  (on-beat-trigger 64 #(do (spin-for (rand-int voices) durations (:duration ss))))
  (remove-all-beat-triggers)

  (def ss (sample->smooth [rf-solve-s rf-full-s rf-theorems-s rf-full-s rf-fx-s] voices pattern-size))
  (pattern! (:duration ss) (take voices (repeatedly #(rand-nth durations))))
  (pattern! (:amp ss)      (take pattern-size (repeatedly #(ranged-rand 0.1 0.2))))
  (pattern! (:fraction ss) (take pattern-size (repeatedly #(/ (rand 512) 512))))

  (def ss (sample->smooth [] voices pattern-size smooth-samples))
  (pattern! (:duration ss) [1/128])
  (pattern! (:duration ss) [1/4])
  (pattern! (:duration ss) [1/4 0 0 0 1/4 0 0 0])
  (pattern! (:duration ss) [1/12 0 0 0 0 0 0 0])
  (pattern! (:duration ss) [1/12 0 0 0 1/4 0 0 0])
  (pattern! (:duration ss) [1/2])

  (pattern! (:amp ss)      [0.18 0.18 0.18 0.18 0.18 0.18 0.18 0.18])

  (pattern! (:fraction ss) [0.82283354 0.45919186 0.54692537 0.0045858636 0.034107555 0.6987561 0.07871687 0.24623081])
  (pattern! (:fraction ss) [0.8845941 0.3484526 0.02742675 0.82377213 0.7945769 0.772626 0.45249504 0.35252455])
  (pattern! (:fraction ss) [0.2470634 0.5662428 0.63178784 0.9357417 0.66654444 0.0969285 0.40005338 0.675227])

  (def gs (sample->percussive [rf-solve-s rf-full-s rf-theorems-s rf-full-s rf-fx-s] voices pattern-size))
  (def gs (sample->percussive perc-samples voices pattern-size))

  (buffer-write! (:duration gs) (take voices (repeatedly #(rand-nth durations))))
  (buffer-write! (:amp gs)      (take pattern-size (repeatedly #(ranged-rand 0.3 0.5))))
  (buffer-write! (:fraction gs) (take voices (repeatedly #(/ (rand 512) 512))))

  (pattern! (:duration gs) [1/64 1/2 1/2 1/2 1/64 1/2 1/2 1/2])
  (pattern! (:duration gs) [1/3 1/4 1/2 1/2 1/4 0 1/4 1/4])
  (pattern! (:amp gs)      [0.4 0.1 0.1 0.1 0.1 0.1 0.1 0.1])

  (pattern! (:fraction gs) [1 0.9 0.1 0.1 0.1 0.1 0.1 0.1])
  (pattern! (:fraction gs) [0.14313303 0.641848 0.79618585 0.3601217 0.8650944 0.5890187 0.2760824 0.116221964])
  )

(comment
  (do ;;init graphics
    (def beats (buffer->tap kick-seq-buf (:count time/beat-1th) :measure 8))
    (defonce circle-count        (atom 4.0))
    (defonce color               (atom 0.1))
    (defonce circle-destruction  (atom 8.0))
    (defonce circle-growth-speed (atom 0.1))
    (defonce circle-destructure  (atom 1.0))

    (defonce circular-weight   (atom 0.0))
    (defonce flare-weight      (atom 0.0))
    (defonce population-weight (atom 0.0))
    (defonce cells-weight      (atom 0.0))
    (defonce nyc-weight        (atom 0.0))
    (defonce invert-color      (atom 1.0))
    (defonce cell-dance-weight (atom 0.0)))

  ;;(kill beats)
  (t/start "resources/shaders/nyc.glsl"
           :textures [:overtone-audio :previous-frame
                      "resources/textures/tex16.png"]
           :user-data {"iMeasureCount"   (atom {:synth beats :tap "measure-count"})
                       "iBeatTotalCount" (atom {:synth beats :tap "beat-total-count"})
                       "iGlobalBeatCount" (atom {:synth beats :tap "global-beat-count"})
                       "iBeat"           (atom {:synth beats :tap "beat"})
                       "iBeatCount"      (atom {:synth beats :tap "beat-count"})
                       "iColor" color
                       "iCircleCount" circle-count
                       "iHalfPi" circle-destruction
                       "iInOutSpeed" circle-growth-speed
                       "iDestructure" circle-destructure
                       "iCircularWeight"  circular-weight
                       "iFlareWeight"      flare-weight
                       "iPopulationWeight" population-weight
                       "iBouncingWeight"   cells-weight
                       "iNycWeight" nyc-weight
                       "iInvertColor" invert-color
                       "iCircleDanceWeight" cell-dance-weight
                       })

  (t/stop)
  (stop-everything!)
  (stop)
)
