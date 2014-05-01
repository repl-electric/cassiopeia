(ns cassiopeia.view-screen
  (:use [overtone.live])
  (:require [shadertone.tone :as t]))

;;(demo (sin-osc))

(defonce color-l (atom 0.0))
(defonce color-r (atom 0.0))
(defonce res     (atom 0.75))
(defonce space   (atom 0.1))
(defonce expand  (atom 0.0))
(defonce yinyan  (atom 0.0))

(defonce cutout-w (atom 0.0))
(defonce stars-w  (atom 0.0))
(defonce heart-w  (atom 0.0))
(defonce hyper-w  (atom 0.0))

(defn activate-eta-view-screen [beats]
  (t/start-fullscreen "resources/shaders/electric.glsl"
                      :textures [:overtone-audio :previous-frame
                                 "resources/textures/repl-electric-t.png"
                                 "resources/textures/tex16.png"]
                      :user-data {"iMixRate" color-l "iColorStrength" color-r "iRes" res
                                  "iSpace" space "iExpand" expand "iYinYan" yinyan
                                  "iCutoutWeight"      cutout-weight
                                  "iSpaceLightsWeight" stars-w
                                  "iDistortedWeight"   heart-w
                                  "iSpaceyWeight"      hyper-w
                                  "iMeasureCount" (atom {:synth beats :tap "measure-count"})
                                  "iBeat"         (atom {:synth beats :tap "beat"})
                                  "iBeatCount"    (atom {:synth beats :tap "beat-count"})})
 )

(comment
  (t/start-fullscreen "resources/shaders/zoomwave.glsl"
                      :textures [ :overtone-audio :previous-frame]
                      :user-data {"iLColor" color-l "iRColor" color-r
                                  "iA" (atom {:synth s :tap "a"})})

  (t/start-fullscreen "resources/shaders/wave.glsl"     :textures [ :overtone-audio])
  (t/stop)
  (stop))
