(ns cassiopeia.space
"
██████╗ ███████╗██████╗ ██╗         ███████╗██╗     ███████╗ ██████╗████████╗██████╗ ██╗ ██████╗
██╔══██╗██╔════╝██╔══██╗██║         ██╔════╝██║     ██╔════╝██╔════╝╚══██╔══╝██╔══██╗██║██╔════╝
██████╔╝█████╗  ██████╔╝██║         █████╗  ██║     █████╗  ██║        ██║   ██████╔╝██║██║
██╔══██╗██╔══╝  ██╔═══╝ ██║         ██╔══╝  ██║     ██╔══╝  ██║        ██║   ██╔══██╗██║██║
██║  ██║███████╗██║     ███████╗    ███████╗███████╗███████╗╚██████╗   ██║   ██║  ██║██║╚██████╗
╚═╝  ╚═╝╚══════╝╚═╝     ╚══════╝    ╚══════╝╚══════╝╚══════╝ ╚═════╝   ╚═╝   ╚═╝  ╚═╝╚═╝ ╚═════╝
"
(:use [overtone.live] [cassiopeia.waves.synths] [cassiopeia.engine.core] [cassiopeia.waves.soprano] [cassiopeia.view-screen] [cassiopeia.engine.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.dirt] [cassiopeia.engine.samples])
(:require [cassiopeia.engine.timing :as time] [overtone.studio.fx :as fx]  [shadertone.tone :as t] [cassiopeia.engine.buffers :as b]))

(ctl time/root-s :rate 4)
