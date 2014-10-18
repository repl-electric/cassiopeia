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
(pattern! hats-buf      [0])
(pattern! white-seq-buf [0])

(def beats (buffer->tap kick-seq-buf (:count time/beat-1th)))

(do (reset! color-l 1.0) (reset! color-r 1.0) (reset! expand 1.0) (reset! stars-w 1.0) (reset! yinyan 1.0) (reset! cellular-w 0.0))
(reset! heart-w 0.0)
(reset! cutout-w 0.0)
(reset! cellular-w 0.0)
(reset! no-circles 1.0)
(reset! cutout-w 0.0)
(reset! heart-w 0.0)

(t/stop)
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
