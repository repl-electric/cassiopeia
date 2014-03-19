(ns cassiopeia.view-screen
  (:use [overtone.live])
  (:require [shadertone.tone :as t]))

;;(demo (sin-osc))

(def color-l (atom 0.0))
(def color-r (atom 0.0))

(t/start-fullscreen "resources/shaders/zoomwave.glsl" :textures [ :overtone-audio :previous-frame] :user-data {"iLColor" color-l
                                                                                                               "iRColor" color-r})
(comment
  (t/start-fullscreen "resources/shaders/wave.glsl"     :textures [ :overtone-audio])
  (t/stop)
  (stop))
