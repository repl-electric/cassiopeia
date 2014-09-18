(ns cassiopeia.space
"
██████╗ ███████╗██████╗ ██╗         ███████╗██╗     ███████╗ ██████╗████████╗██████╗ ██╗ ██████╗
██╔══██╗██╔════╝██╔══██╗██║         ██╔════╝██║     ██╔════╝██╔════╝╚══██╔══╝██╔══██╗██║██╔════╝
██████╔╝█████╗  ██████╔╝██║         █████╗  ██║     █████╗  ██║        ██║   ██████╔╝██║██║
██╔══██╗██╔══╝  ██╔═══╝ ██║         ██╔══╝  ██║     ██╔══╝  ██║        ██║   ██╔══██╗██║██║
██║  ██║███████╗██║     ███████╗    ███████╗███████╗███████╗╚██████╗   ██║   ██║  ██║██║╚██████╗
╚═╝  ╚═╝╚══════╝╚═╝     ╚══════╝    ╚══════╝╚══════╝╚══════╝ ╚═════╝   ╚═╝   ╚═╝  ╚═╝╚═╝ ╚═════╝
"
(:use [overtone.live] [cassiopeia.waves.synths] [mud.core] [cassiopeia.waves.soprano] [cassiopeia.view-screen] [mud.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.dirt] [cassiopeia.engine.samples])
(:require [mud.timing :as time] [overtone.studio.fx :as fx]  [shadertone.tone :as t] [cassiopeia.engine.buffers :as b]))

(ctl time/root-s :rate 4)

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 96 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf])
    (pattern! kick-seq-buf  [0])
    (pattern! bass-notes-buf (repeat 2 (repeat 4 [:B1 :B1 :B1 :B1]))
              (repeat 2 (repeat 4 [:E#1 :E#1 :E#1 :E#1]))
              (repeat 2 (repeat 4 [:F#1 :F#1 :F#1 :F#1]))))

(pattern! kick-seq-buf  [0])
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

(def beats (buffer->tap kick-seq-buf (:count time/beat-1th)))

(do (reset! color-l 1.0) (reset! color-r 1.0) (reset! expand 1.0) (reset! stars-w 1.0) (reset! yinyan 1.0) (reset! cellular-w 0.0))
(reset! heart-w 0.0)
(reset! cutout-w 0.0)
(reset! cellular-w 0.0)

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
