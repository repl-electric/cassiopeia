(ns cassiopeia.destination.eta
  "
 ___
 )_ _)_ _
(__ (_ (_(

Eta Cassiopeiae is a star system in the northern circumpolar constellation of Cassiopeia.
"
(:require [cassiopeia.engine.timing :as time]
          [overtone.studio.fx :as fx]
          [cassiopeia.engine.mixers :as mix]
          [overtone.inst.synth :as s]
          [shadertone.tone :as t])
(:use [overtone.live]
      [cassiopeia.engine.core]
      [cassiopeia.samples]
      [cassiopeia.view-screen]
      [cassiopeia.waves.synths]))
(stop)
(do
  (ctl time/root-s :rate 4)

  (defonce voice-g     (group "main voice"))
  (defonce bass-g      (group "bass voice"))
  (defonce drums-g (group "drums"))

  (defonce pulsar-buf            (buffer 128))

  (defonce bass-notes-buf (buffer 128))

  (defonce fizzy-note-buf         (buffer 128))

  (defonce phase-bass-buf            (buffer 128))
  (defonce shrill-buf            (buffer 128))
  (defonce growl-buf             (buffer 128))
  (defonce growl2-buf            (buffer 128))
  (defonce white-seq-buf         (buffer 24))
  (defonce shrill-dur-buf        (buffer 32))
  (defonce fizzy-dur-buf         (buffer 128))
  (defonce shrill-pong-buf       (buffer 128))
  (defonce shrill-pong-final-buf (buffer 128))

  (defonce kick-seq-buf          (buffer 96))
  (defonce bass-notes-buf        (buffer 96)))

(def hats
    (doall (map #(high-hats
                  [:head drums-g]
                  :amp 0.09
                  :mix (nth (take 32 (cycle [1.0 1.0])) %1)
                  :room 4
                  :note-buf bass-notes-buf
                  :seq-buf phase-bass-buf
                  :beat-bus     (:count time/beat-1th)
                  :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.9 :mix 0.9 :room 50 :amp 0.1)

(doseq [i (range 0 96)]
  (kick2
   [:head drums-g]
   :note-buf bass-notes-buf
   :seq-buf  kick-seq-buf
   :beat-bus      (:count time/beat-1th)
   :beat-trg-bus  (:beat time/beat-1th)
   :num-steps 96
   :beat-num i))

(kill drums-g)

(ctl drums-g :mod-freq 1)
(ctl drums-g :mod-index 1.3 :noise 900)

(pattern! bass-notes-buf
          (repeat 5 [:A1])
          (repeat 2 [:A2]))
(pattern! phase-bass-buf (repeat 4 (repeat 4 [1 0 0 0])) [1 1 1 1])
(pattern! kick-seq-buf
          (repeat 4 (repeat 4 [1 0 0 0]))
          (repeat 2 (repeat 4 [1 1 0 0])))
(pattern! bass-notes-buf
          (repeat 4 (repeat 4 [:A1 :A1 :A1 :A1]))
          (repeat 2 (repeat 4 [:E#1 :E#1 :E#1 :E#1])))

(def white (doall (map
                   #(whitenoise-hat
                     [:head drums-g]
                     :amp 0.2
                     :seq-buf  white-seq-buf
                     :beat-bus     (:count time/beat-1th)
                     :beat-trg-bus (:beat time/beat-1th)
                     :num-steps 24
                     :beat-num %1) (range 0 24))))

(pattern! white-seq-buf [1])
(pattern! white-seq-buf (repeat 4 [1 0 0 0]) [1 1 1 1])

(def s (shrill-pong [:head voice-g] :amp 0.2 :note-buf shrill-pong-buf :duration-bus shrill-dur-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))

(ctl s :amp 1.1)

(pattern! shrill-dur-buf
          (repeat 4 (repeat 4 [1/64 1/64 1/64 1/64]))
          (repeat 4 [1/128 1/128 1/64 1/64])
          )

(pattern! shrill-dur-buf
          (repeat 4 (repeat 4 [1/12 1/12 1/2 1/2]))
          (repeat 4 (repeat 4 [1/12 1/12 1/12 1/4]))
          (repeat 4 [1/24 1/2 1/24 1/2]))

(pattern! shrill-dur-buf
          (repeat 4 (repeat 4 [1/12 1/12 1/12 1/4]))
          (repeat 4 [1/4 1/4 1/4 1/4]))

(pattern! shrill-dur-buf
          (repeat 4 (repeat 2 [1/2 1/4 1/4 1/2 1/4 1/4 1/2 1/8]))
          (repeat 4 [1/2 1/2 1/2 1/2]))

(def p (pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf pulsar-buf :amp 0.7))

(def fizzy-p (fizzy-pulsar :amp 0.6 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf fizzy-note-buf :duration-bus shrill-dur-buf))

(let [octive 1
      [n1 n2 n3 n4]     (chord-degree :v (note-at-octave :A octive)       :major)
      [n11 n12 n13 n14] (chord-degree :i (note-at-octave :A (inc octive)) :major)]
  (pattern! pulsar-buf
            (repeat 4 (repeat 4 [0 0 0 0]))
            (repeat 4 [(note-at-octave :F# (+ 2 octive)) (note-at-octave :F# (+ 2 octive))  0 0])
            (repeat 4 [(note-at-octave :G# (+ 2 octive))  (note-at-octave :G# (+ 2 octive)) 0 0]))
  (pattern! shrill-buf
            (repeat 4 (repeat 4 [n1 n1 n3 0]))
            (repeat 4 [n3 n3 n4 0]))
  (pattern! shrill-pong-buf
            (repeat 3 [n1 n2 n2 n3])
            (repeat 1 [n11 n13 n14 n14])
            (repeat 3 [n1 n2 0 0])
            (repeat 1 [n11 n13 n14 69]))
  (pattern! fizzy-note-buf
            (repeat 3 [n1 n1 n1 n1])
            (repeat 1 [0 0 0 0])
            (repeat 3 [n2 n2 n2 n2])
            (repeat 1 [0 0 0 0])
            (repeat 4 (repeat 4 [0 0 0 0])))
  )

(def growl-synth (growl [:head bass-g] :amp 1 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf growl-buf))

(node-over-time growl-synth :amp  1 0.0 0.01)

(pattern! growl-buf (degrees [1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1
                              3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3] :major :A2))

(def growl-synth (growl [:head bass-g] :amp 1 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf growl2-buf))

(pattern! growl2-buf (degrees [1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1
                              3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                              5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                              6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                              7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7
                              8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8] :major :A3))

(stop)
