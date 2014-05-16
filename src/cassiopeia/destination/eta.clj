(ns cassiopeia.destination.eta "███████╗████████╗ █████╗
                                ██╔════╝╚══██╔══╝██╔══██╗
                                █████╗     ██║   ███████║
                                ██╔══╝     ██║   ██╔══██║
                                ███████╗   ██║   ██║  ██║
                                ╚══════╝   ╚═╝   ╚═╝  ╚═╝"
(:require [cassiopeia.engine.timing :as time] [overtone.studio.fx :as fx] [cassiopeia.engine.mixers :as mix] [overtone.inst.synth :as s] [shadertone.tone :as t] [cassiopeia.engine.buffers :as b]) (:use [overtone.live] [cassiopeia.engine.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.engine.samples] [cassiopeia.view-screen] [cassiopeia.waves.synths] [cassiopeia.waves.soprano]))
(do (ctl time/root-s :rate 4)
;;    (ctl (foundation-output-group) :master-volume 1)
    (defonce voice-g (group "main voice")) (defonce backing-voice-g (group "backing voices")) (defonce bass-g  (group "bass voice")) (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 96 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf stella-wind-note-buf nebula-note-buf supernova-dur-buf supernova-note-buf helium-note-buf hydrogen-note-buf supernova-dur-buf helium-dur-buf hydrogen-dur-buf metallicity-note-buf]))

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
(pattern! kick-seq-buf  (repeat 5 [1 0 0 0  1 0 0 0  1 0 0 1  1 0 1 1])
                        (repeat 1 [1 0 0 0  1 0 0 0  0 0 0 1  1 1 1 1]))

(def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 1)))
(ctl drums-g :mod-freq 10.2 :mod-index 0.1 :noise 0)

(def ghostly-snares (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.2 :num-steps 16 :buf (b/buffer-mix-to-mono snare-ghost-s)) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf (b/buffer-mix-to-mono deep-bass-kick-s)) (range 0 8))))

(def hats (doall (map #(high-hats [:head drums-g] :amp 0.2 :mix (nth (take 32 (cycle [1.0 1.0])) %1) :room 4 :note-buf bass-notes-buf :seq-buf hats-buf :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.9 :mix 0.2 :room 10 :amp 0.2)

(def white-hats (doall (map #(whitenoise-hat [:head drums-g] :amp 0.2 :seq-buf  white-seq-buf :num-steps 16 :beat-num %1) (range 0 16))))

(def nebula (growl [:head bass-g] :amp 0.0 :beat-trg-bus (:beat time/beat-16th) :beat-bus (:count time/beat-16th) :note-buf nebula-note-buf))

(fadein nebula)
(pattern-at! nebula-note-buf time/main-beat 32 (degrees [] :major :A2))

(pattern! hydrogen-dur-buf  (repeat 4 [1/8 1/8 1/2 1/2]) (repeat 4 [1/12 1/12 1/12 1/12]))
(pattern! hydrogen-note-buf (degrees [] :major :A2))

(def hydrogen  (shrill-pong [:head voice-g] :amp 1.2 :note-buf hydrogen-note-buf :duration-bus hydrogen-dur-buf))
(def helium    (shrill-pong [:head voice-g] :amp 1.2 :note-buf helium-note-buf :duration-bus helium-dur-buf))
(def supernova (shrill-pong [:head voice-g] :amp 0.1 :note-buf supernova-note-buf :duration-bus supernova-dur-buf))

(fadeout hydrogen)
(n-overtime! supernova :amp 0.1 1.2 0.01)

(pattern! helium-dur-buf    (repeat 16 [1/9]) (repeat 4 (repeat 16 [1/8])))
(pattern! supernova-dur-buf (repeat 4 (repeat 2 [1/2 1/4 1/2 1/2 1/4 1/2 1/2 1/12])) (repeat 4 [1/2 1/2 1/2 1/2]))

(def stellar-wind (pulsar :note-buf stella-wind-note-buf :amp 0.7))
(def metallicity (fizzy-pulsar [:head backing-voice-g] :amp 0.6 :note-buf metallicity-note-buf :duration-bus supernova-dur-buf))

(let [octave 2
      [n1 n2 n3 n4]     (chord-degree :v (note-at-octave :A octave) :major)
      [n11 n12 n13 n14] (chord-degree :i (note-at-octave :A (if (> octave 3) octave (inc octave))) :major)]
  (pattern! stella-wind-note-buf
            (repeat 4 (repeat 4 [0 0 0 0]))
            (repeat 4 [(note-at-octave :F# (+ (if (> octave 3) 0 2) octave)) (note-at-octave :F# (+ (if (> octave 3) 0 2) octave))  0 0])
            (repeat 2 [(note-at-octave :G# (+ (if (> octave 3) 0 2) octave)) (note-at-octave :G# (+ (if (> octave 3) 0 2) octave)) 0 (note-at-octave :G# (+  (if (> octave 3) 0 2) octave))])
            (repeat 2 [(note-at-octave :G# (+ (if (> octave 3) 0 2) octave)) (note-at-octave :G# (+ (if (> octave 3) 0 2) octave)) 0  0 ]))
  (pattern! supernova-note-buf
            (repeat 4 [n1 n3 n3 n3])
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
            (repeat 2 [n13 n13 n14 n14])  [n3 n3 n1 n1] [n1 n2 n3 n3] [n1 n1 n13 n13]
            [n1 n2 n3 n3] [n3 n3 n1 n1]   [n1 n2 n3 n3] [n1 n1 n3 n3]
            (repeat 4 [n14 n13 n12 (if (> octave 3) n14 (inc n14))]))
  (pattern! helium-note-buf
            (degrees [8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8
                      7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7
                      6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                      5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                      3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                      1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1]
                     :major (note-at-octave :A (cond (= octave 1) octave
                                                     true (dec octave)))))
  (pattern! metallicity-note-buf
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

(do (reset! color-l 1.0) (reset! color-r 1.0) (reset! expand 1.0) (reset! stars-w 1.0) (reset! yinyan 1.0) (reset! cellular-w 0.0))
(reset! heart-w 0.0)
(reset! cutout-w 0.0)
(reset! cellular-w 0.0)

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
                       "iCellGrowth"        cellular-growth
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
  (ctl supernova :amp 0)
  (ctl helium :amp 0)
  (ctl hydrogen :amp 0)
  (ctl stellar-wind :amp 0)
  (ctl metallicity :amp 0)
  (ctl nebula :amp 0)
  )

(defn full-stop []
  (reset! cutout-w 0.0)
  (reset! stars-w 0.0)
  (reset! heart-w 0.0)
  (remove-on-beat-trigger)
  (fadeout-master))
