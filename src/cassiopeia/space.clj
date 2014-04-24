(ns cassiopeia.space
"
Repl Electric
"
(:use [overtone.live] [cassiopeia.waves.synths] [cassiopeia.engine.core] [cassiopeia.waves.soprano]       [cassiopeia.view-screen])
(:require [cassiopeia.engine.timing :as time] [overtone.studio.fx :as fx]  [shadertone.tone :as t]))

(ctl time/root-s :rate 4)
