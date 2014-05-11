(ns cassiopeia.destination.eta
"___
 )_ _)_ _
(__ (_ (_(
Eta Cassiopeia is a star system in the northern circumpolar constellation of Cassiopeia.
"
(:require [cassiopeia.engine.timing :as time] [overtone.studio.fx :as fx] [cassiopeia.engine.mixers :as mix] [overtone.inst.synth :as s] [shadertone.tone :as t] [cassiopeia.engine.buffers :as b])
(:use [overtone.live] [cassiopeia.engine.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.engine.samples] [cassiopeia.view-screen] [cassiopeia.waves.synths] [cassiopeia.waves.soprano]))

(do
  (ctl time/root-s :rate 4)
  (defonce voice-g (group "main voice"))
  (defonce backing-voice-g (group "backing voices"))
  (defonce bass-g  (group "bass voice"))
  (defonce drums-g (group "drums"))
  (defonce drum-effects-g (group "drums effects"))
  (defbufs 96 [pulsar-buf bass-notes-buf hats-buf growl-buf white-seq-buf shrill-buf shrill-dur-buf shrill-pong-buf shrill-pong2-buf shrill-pong3-buf shrill-dur-buf shrill-dur2-buf shrill-dur3-buf kick-seq-buf bass-notes-buf fizzy-note-buf fizzy-dur-buf effects-seq-buf effects2-seq-buf]))

(pattern! kick-seq-buf  [1 0 0 0 0 0 0 0])
(pattern! hats-buf      [0 0 0 0 0 0 1 1])
(pattern! white-seq-buf [0 1 1 0 1 0 1 1])

(pattern! hats-buf        (repeat 4 [1 0 0 0]) (repeat 4 [1 1 0 0]))
(pattern! kick-seq-buf    (repeat 6 [1 0 0 0]) (repeat 2 [1 0 1 1]))
(pattern! white-seq-buf   (repeat 3 [1 0 0 0]) [1 1 1 0])
(pattern! effects-seq-buf (repeat 4 [1 0 0 0]))
(pattern! effects-seq-buf [1 0 0 0] (repeat 3 [0 0 0 0]))
(pattern! effects2-seq-buf [0 0 0 0] [1 1 1 1] [0 0 0 0] [1 0 1 1])

(pattern! white-seq-buf (repeat 3 [1 0 0 0]) [1 1 1 1])
(pattern! hats-buf      (repeat 6 (concat (repeat 3 [0 1 0 0]) [1 1 0 0])))
(pattern! kick-seq-buf  (repeat 5 (repeat 4 [1 0 1 1])) (repeat 4 [1 1 1 1]))
(pattern! kick-seq-buf
          (repeat 5 [1 0 0 0  1 0 0 0  1 0 0 1  1 0 1 1])
          (repeat 1 [1 0 0 0  1 0 0 0  0 0 0 1  1 1 1 1]))

;;(kill drum-effects-g)
;;(kill drums-g)
(def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 1)))
(ctl drums-g :mod-freq 10.2 :mod-index 0.1 :noise 0)

(def ghostly-snares (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.2 :num-steps 16 :buf (b/buffer-mix-to-mono snare-ghost-s)) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf (b/buffer-mix-to-mono deep-bass-kick-s)) (range 0 8))))

(def hats (doall (map #(high-hats [:head drums-g] :amp 0.2 :mix (nth (take 32 (cycle [1.0 1.0])) %1) :room 4 :note-buf bass-notes-buf :seq-buf hats-buf :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.9 :mix 0.2 :room 10 :amp 0.2)

(def white (doall (map #(whitenoise-hat [:head drums-g] :amp 0.2 :seq-buf  white-seq-buf :num-steps 16 :beat-num %1) (range 0 16))))

(def growl-synth (growl [:head bass-g] :amp 0.0 :beat-trg-bus (:beat time/beat-16th) :beat-bus (:count time/beat-16th) :note-buf growl-buf))

(fadein growl-synth)
(pattern-at! growl-buf time/main-beat 32 (degrees [1 3] :major :A2))

(pattern! shrill-dur3-buf
          (repeat 4 [1/8 1/8 1/2 1/2])
          (repeat 4 [1/12 1/12 1/12 1/12]))

(pattern! shrill-pong3-buf (degrees [3 3 3 3  3 3 3 3  3 3 3 3   3 3 3 3
                                     5 5 5 5  5 5 5 5  5 5 5 5   5 5 5 5
                                     ] :major :A2))

(def s3 (shrill-pong [:head voice-g] :amp 1.2 :note-buf shrill-pong3-buf :duration-bus shrill-dur3-buf))

(def s2 (shrill-pong [:head voice-g] :amp 1.2 :note-buf shrill-pong2-buf :duration-bus shrill-dur2-buf ))

(def s1 (shrill-pong [:head voice-g] :amp 0.1 :note-buf shrill-pong-buf :duration-bus shrill-dur-buf))

(fadeout s3)
(n-overtime! s1 :amp 0.1 1.2 0.01)

(pattern! shrill-dur2-buf
          (repeat 16 [1/9])
          (repeat 4 (repeat 16 [1/8])))

(pattern! shrill-dur-buf
          (repeat 4 (repeat 2 [1/2 1/4 1/2 1/2 1/4 1/2 1/2 1/12]))
          (repeat 4 [1/2 1/2 1/2 1/2]))

(def pip (pulsar [:head backing-voice-g] :note-buf pulsar-buf :amp 0.7))
(def fizzy-p (fizzy-pulsar [:head backing-voice-g] :amp 0.6 :note-buf fizzy-note-buf :duration-bus shrill-dur-buf))

(let [octave 2
      [n1 n2 n3 n4]     (chord-degree :v (note-at-octave :A octave)       :major)
      [n11 n12 n13 n14] (chord-degree :i (note-at-octave :A (inc octave)) :major)]
  (pattern! pulsar-buf
            (repeat 4 (repeat 4 [0 0 0 0]))
            (repeat 4 [(note-at-octave :F# (+ 2 octave)) (note-at-octave :F# (+ 2 octave))  0 0])
            (repeat 2 [(note-at-octave :G# (+ 2 octave)) (note-at-octave :G# (+ 2 octave)) 0 (note-at-octave :G# (+ 2 octave))])
            (repeat 2 [(note-at-octave :G# (+ 2 octave))  (note-at-octave :G# (+ 2 octave)) 0  0 ]))
  (pattern! shrill-buf
            (repeat 4 (repeat 4 [n1 n1 n3 0]))
            (repeat 4 [n3 n3 n4 0]))
  (pattern! shrill-pong-buf
            (repeat 4 [n1 n3 n3 n3])
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
            (repeat 2 [n13 n13 n14 n14])  [n3 n3 n1 n1] [n1 n2 n3 n3] [n1 n1 n13 n13]
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
            (concat (repeat 3 [n14 n13 n12 (inc n14)])))
  (pattern! shrill-pong2-buf
            (degrees [8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8
                      7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7
                      6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                      5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                      3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                      1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1]
                     :major (note-at-octave :A (cond (= octave 1) octave
                                                     true (dec octave)))))
  (pattern! fizzy-note-buf
            (repeat 3 [n1 n1 n1 n1])
            (repeat 1 [0 0 0 0])
            (repeat 3 [n2 n2 n2 n2])
            (repeat 1 [0 0 0 0])
            (repeat 4 (repeat 4 [0 0 0 0]))))

;;(stop)

(pattern! kick-seq-buf  [0])
(pattern! bass-notes-buf
          (repeat 2 (repeat 4 [:B1 :B1 :B1 :B1]))
          (repeat 2 (repeat 4 [:E#1 :E#1 :E#1 :E#1]))
          (repeat 2 (repeat 4 [:F#1 :F#1 :F#1 :F#1])))

(do
  (reset! color-l 1.0) (reset! color-r 1.0) (reset! expand 1.0) (reset! stars-w 1.0) (reset! yinyan 1.0) (reset! cellular-w 1.0))

;;(stop)

(comment
  (def beats (buffer->tap kick-seq-buf (:count time/beat-1th)))

  (reset! heart-w 1.0)
  (reset! stars-w 0.0)

  (t/start-fullscreen "resources/shaders/electric.glsl"
           :textures [:overtone-audio :previous-frame
                      "resources/textures/repl-electric-t.png"
                      "resources/textures/tex16.png"]
           :user-data {"iMixRate" color-l "iColorStrength" color-r "iRes" res
                       "iSpace" space "iExpand" expand "iYinYan" yinyan
                       "iCircleCount" no-circles "iStarDirection" stars-direction
                       "iCutoutWeight"      cutout-w
                       "iSpaceLightsWeight" stars-w
                       "iDistortedWeight"   heart-w
                       "iSpaceyWeight"      hyper-w
                       "iCellularWeight"    cellular-w
                       "iMeasureCount" (atom {:synth beats :tap "measure-count"})
                       "iBeat"         (atom {:synth beats :tap "beat"})
                       "iBeatCount"    (atom {:synth beats :tap "beat-count"})})

  ;;(t/stop)
  (reset! color-l 1.0)
  (reset! color-r 1.0)
  (reset! expand 1.0)
  (reset! yinyan 1.0)
  (reset! res 1.0)
  (reset! color-l 0.0)
  (reset! space 0.5)

  (overtime! stars-direction 10.0 0.001)

  (reset! cutout-w 0.0)
  (reset! heart-w 1.0)
  (reset! stars-w 0.0)

  (reset! no-circles 1.0)

  (kill drums-g)
  (kill voice-g)
  (kill backing-voice-g)
  (kill bass-g)
  (ctl drums-g :amp 0)
  (ctl drum-effects-g :amp 0)
  (ctl s3 :amp 0)
  (ctl s2 :amp 0)
  (ctl s1 :amp 0)
  (ctl pip :amp 0)
  (ctl fizzy-p :amp 0)
  (ctl growl-synth :amp 0)
  )
(defn full-stop []
  (reset! cutout-w 0.0)
  (reset! stars-w 0.0)
  (reset! heart-w 0.0)
  (remove-on-beat-trigger)
  (stop))
