(ns cassiopeia.destination.eta
"___
 )_ _)_ _
(__ (_ (_(
Eta Cassiopeia is a star system in the northern circumpolar constellation of Cassiopeia.
"
(:require [cassiopeia.engine.timing :as time] [overtone.studio.fx :as fx] [cassiopeia.engine.mixers :as mix] [overtone.inst.synth :as s] [shadertone.tone :as t])
(:use [overtone.live] [cassiopeia.engine.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.engine.samples] [cassiopeia.view-screen] [cassiopeia.waves.synths]))

(do
  (ctl time/root-s :rate 4)
  (defonce voice-g (group "main voice"))
  (defonce backing-voice-g (group "backing voices"))
  (defonce bass-g  (group "bass voice"))
  (defonce drums-g (group "drums"))

  (defbufs 96 [pulsar-buf bass-notes-buf hats-buf growl-buf
               white-seq-buf shrill-buf shrill-dur-buf
               shrill-pong-buf shrill-pong2-buf shrill-pong3-buf
               shrill-dur-buf shrill-dur2-buf shrill-dur3-buf
               kick-seq-buf bass-notes-buf fizzy-note-buf fizzy-dur-buf]))

(pattern! hats-buf     (repeat 4 [1 0 0 0]) (repeat 4 [1 1 1 1]))
(pattern! kick-seq-buf (repeat 6 [1 0 0 0]) (repeat 2 [1 0 1 1]))
(pattern! bass-notes-buf
          (repeat 2 (repeat 4 [:B1 :B1 :B1 :B1]))
          (repeat 2 (repeat 4 [:E#1 :E#1 :E#1 :E#1]))
                    (repeat 2 (repeat 4 [:F#1 :F#1 :F#1 :F#1])))

(pattern! kick-seq-buf  [1 0 0 0 0 0 0 0])
(pattern! hats-buf      [0 0 0 0 0 0 1 1])
(pattern! white-seq-buf [0 1 1 0 1 0 1 1])

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

(def hats
    (doall (map #(high-hats
                  [:head drums-g]
                  :amp 0.2
                  :mix (nth (take 32 (cycle [1.0 1.0])) %1)
                  :room 4
                  :note-buf bass-notes-buf
                  :seq-buf hats-buf
                  :beat-bus     (:count time/beat-1th)
                  :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.9 :mix 0.9 :room 50 :amp 0.2)

(pattern! bass-notes-buf (repeat 5 [:A1]) (repeat 2 [:A2]))
(pattern! hats-buf       (repeat 6 (concat (repeat 3 [0 1 0 0]) [1 1 0 0] )))
(pattern! kick-seq-buf   (repeat 5 (repeat 4 [1 0 1 1])) (repeat 4 [1 1 1 1]))
(pattern! kick-seq-buf
          (repeat 1 [1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 1])
          (repeat 1 [1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 1])
          (repeat 1 [1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 1])
          (repeat 1 [1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 1])
          (repeat 1 [1 0 0 0 1 0 0 0 1 0 0 1 1 0 1 1])
          (repeat 1 [1 0 0 0 1 0 0 0 0 0 0 1 1 1 1 1]))

(def white (doall (map
                   #(whitenoise-hat
                     [:head drums-g]
                     :amp 0.2
                     :seq-buf  white-seq-buf
                     :beat-bus     (:count time/beat-1th)
                     :beat-trg-bus (:beat time/beat-1th)
                     :num-steps 24
                     :beat-num %1) (range 0 24))))

(pattern! white-seq-buf [1 0 ])
(pattern! white-seq-buf (repeat 3 [1 0 0 0]) [1 1 1 1])

(def growl-synth (growl [:head bass-g] :amp 0 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf growl-buf))

(fade-in growl-synth)

(pattern-at! growl-buf time/main-beat 32
             (degrees [1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1
                       3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                       ;;                      5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                       ;;                      6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                       ;;                      7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7
                       ;;                      8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8
                       ] :major :A2))

(pattern! shrill-dur3-buf (repeat 4 [1/8 1/8 1/2 1/2])
          (repeat 4 [1/12 1/12 1/12 1/12]))

(pattern! shrill-pong3-buf (degrees [3 3 3 3  3 3 3 3  3 3 3 3   3 3 3 3
                                     5 5 5 5  5 5 5 5  5 5 5 5   5 5 5 5
                                     ] :major :A2))

(def s3 (shrill-pong [:head voice-g] :amp 1.2 :note-buf shrill-pong3-buf :duration-bus shrill-dur3-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))


(def s (shrill-pong [:head voice-g] :amp 0.1 :note-buf shrill-pong-buf :duration-bus shrill-dur-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))

(def s2 (shrill-pong [:head voice-g] :amp 1.2 :note-buf shrill-pong2-buf :duration-bus shrill-dur2-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))

(fade-out s3)
(node-overtime s :amp 0.1 1.2 0.01)

(pattern! shrill-dur2-buf
          (repeat 16 [1/9])
          (repeat 4 (repeat 16 [1/8])))

(pattern! shrill-dur-buf
          (repeat 4 (repeat 2 [1/2 1/4 1/2 1/2 1/4 1/2 1/2 1/12]))
          (repeat 4 [1/2 1/2 1/2 1/2]))

(def p (pulsar [:head backing-voice-g] :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf pulsar-buf :amp 0.7))

(def fizzy-p (fizzy-pulsar [:head backing-voice-g] :amp 0.6 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf fizzy-note-buf :duration-bus shrill-dur-buf))

(let [octave 2
      [n1 n2 n3 n4]     (chord-degree :v (note-at-octave :A octave)       :major)
      [n11 n12 n13 n14] (chord-degree :i (note-at-octave :A (inc octave)) :major)]
  (pattern! pulsar-buf
            (repeat 4 (repeat 4 [0 0 0 0]))
            (repeat 4 [(note-at-octave :F# (+ 2 octave)) (note-at-octave :F# (+ 2 octave))  0 0])
            (repeat 2 [(note-at-octave :G# (+ 2 octave)) (note-at-octave :G# (+ 2 octave)) 0 (note-at-octave :G# (+ 2 octave))])
            (repeat 2 [(note-at-octave :G# (+ 2 octave))  (note-at-octave :G# (+ 2 octave)) 0  0 ])
            )
  (pattern! shrill-buf
            (repeat 4 (repeat 4 [n1 n1 n3 0]))
            (repeat 4 [n3 n3 n4 0]))
  (pattern! shrill-pong-buf
            (repeat 4 [n1 n3 n3 n3])
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
            (repeat 2 [n13 n13 n14 n14])  [n3 n3 n1 n1] [n1 n2 n3 n3] [n1 n1 n13 n13]
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
;;            (repeat 1 [n14 n13 n12 (inc n14)]) [n3 n3 n1 n1] [n1 n2 n3 n3] [n1 n1 n13 n13]
            (repeat 4 [n14 n13 n12 (inc n14)])
            );; [n3 n3 n1 n1] [n1 n2 n3 n3] [n1 n1 n13 n13]
  (pattern! shrill-pong2-buf
            (degrees [8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8
                      7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7
                      6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                      5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                      3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                      1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1] :major (note-at-octave :A (cond
                                                                                     (= octave 1) octave
                                                                                     true  (dec octave)))))
  (pattern! fizzy-note-buf
            (repeat 3 [n1 n1 n1 n1])
            (repeat 1 [0 0 0 0])
            (repeat 3 [n2 n2 n2 n2])
            (repeat 1 [0 0 0 0])
            (repeat 4 (repeat 4 [0 0 0 0]))))

;;(stop)
(pattern! kick-seq-buf  [0])

(do
  (reset! color-l 1.0) (reset! color-r 1.0) (reset! expand 1.0) (reset! stars-w 1.0) (reset! yinyan 1.0))

(stop)

(comment
  (def beats (buffer->tap kick-seq-buf (:count time/beat-1th)))
  (activate-eta-view-screen beats)

  (reset! color-l 1.0)
  (reset! color-r 1.0)
  (reset! expand 1.0)
  (reset! yinyan 1.0)
  (reset! res 1.0)
  (reset! color-l 0.0)
  (reset! space 0.5)

  (t/stop)

  (kill drums-g)
  (kill voice-g)
  (kill backing-voice-g)
  (kill bass-g)
  (ctl drums-g :amp 0)
  (ctl s2 :amp 0)
  (ctl s :amp 0)
  (ctl p :amp 0)
  (ctl fizzy-p :amp 0)
  (ctl growl-synth :amp 0)
  )
