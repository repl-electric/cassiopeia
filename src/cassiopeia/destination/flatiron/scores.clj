(ns cassiopeia.destination.flatiron.scores
  (:use [cassiopeia.engine.expediency]
        [mud.core]
        [mud.chords]
        [overtone.live]))

(def dark-chords-score
  (let [_ [0 0 0 0]
        [f31 f32 f33 f34 f35 f36 f37] (chords-for :F3 :minor 3)
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
    (let [chord-pat
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
     f37 f41 _ f41 c41 _ (as-chord (degrees [1] :minor :F3)) (as-chord (degrees [1] :minor :F3))
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

    [f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
     f41 f43 f41 f44 f37 c36 (as-chord (degrees [7] :minor :F3)) (as-chord (degrees [7] :minor :F3))
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
