(ns stars.destination.demo
  (:use [overtone.live]
        [stars.warm-up]
        [stars.samples]
        [overtone.synth.sampled-piano])
  (:require [stars.engine.timing :as timing]
            [launchpad.sequencer :as lp-sequencer]
            [launchpad.plugin.beat :as lp-beat]
            [overtone.synths :as syn]
            [overtone.inst.synth :as s]))

(ctl timing/b-trg :div 29)

(map-indexed
 #(lp-sequencer/sequencer-write! sequencer-64 %1 %2)
 [[1 0 0 0 1 0 0 0]
  [0 0 0 0 0 0 1 1]
  [0 1 0 0 0 0 0 0]
  [0 0 1 0 0 1 0 0]
  [0 0 0 1 0 0 0 0]
  [0 0 0 0 0 0 0 0]
  [0 0 0 0 0 0 0 0]
  [0 0 0 0 0 0 0 0]])
(lp-beat/grid-pull lp sequencer-64)

(ctl rater-s :rate 0)
(ctl rater-s :rate 1/2)
