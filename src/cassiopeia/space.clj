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
