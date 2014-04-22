(ns cassiopeia.view-screen
  (:use [overtone.live])
  (:require [shadertone.tone :as t]))

;;(demo (sin-osc))

(defonce color-l (atom 0.0))
(defonce color-r (atom 0.0))
(defonce res     (atom 0.75))
(defonce space   (atom 0.1))
(defonce expand   (atom 0.0))
(defonce yinyan (atom 0.0))

(comment
  (t/start-fullscreen "resources/shaders/zoomwave.glsl"
                      :textures [ :overtone-audio :previous-frame]
                      :user-data {"iLColor" color-l "iRColor" color-r
                                  "iA" (atom {:synth s :tap "a"})})

  (t/start-fullscreen "resources/shaders/wave.glsl"     :textures [ :overtone-audio])
  (t/stop)
  (stop))
