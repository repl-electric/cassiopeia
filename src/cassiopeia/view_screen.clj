(ns cassiopeia.view-screen
  (:use [overtone.live])
  (:require [shadertone.tone :as t]))

(demo (sin-osc))

(t/start-fullscreen "resources/shaders/wave.glsl"         :textures [:overtone-audio])
(t/start-fullscreen "resources/shaders/spectrograph.glsl" :textures [:overtone-audio :previous-frame])
(t/start-fullscreen "resources/shaders/zoomwave.glsl"     :textures [:overtone-audio :previous-frame])

(t/stop)
