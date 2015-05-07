(ns cassiopeia.view-screen
  (:use [overtone.live])
  (:require [shadertone.tone :as t]
            [shadertone.shader :as shader]))

;;If we die due to gl errors keep the view screen running.
;;(shader/throw-exceptions-on-gl-errors false)

;;(demo (sin-osc))

(defonce color-l (atom 0.0))
(defonce color-r (atom 0.0))
(defonce res     (atom 0.75))
(defonce space   (atom 0.1))
(defonce expand  (atom 0.0))
(defonce yinyan  (atom 0.0))
(defonce no-circles (atom 1.0))
(defonce stars-direction (atom 1.0))
(defonce cellular-growth (atom 0.0))

(defonce cutout-w (atom 0.0))
(defonce stars-w  (atom 0.0))
(defonce heart-w  (atom 0.0))
(defonce hyper-w  (atom 0.0))
(defonce cellular-w  (atom 0.0))

(comment
  (t/start-fullscreen "resources/shaders/zoomwave.glsl"
                      :textures [ :overtone-audio :previous-frame]
                      :user-data {"iLColor" color-l "iRColor" color-r
                                  "iA" (atom {:synth s :tap "a"})})

  (t/start-fullscreen "resources/shaders/wave.glsl"     :textures [ :overtone-audio])
  (t/stop)
  (stop))
