(ns cassiopeia.warm-up-monome
  (:require [monome.core :as mon]
            [monome.fonome :as fon]
            [monome.polynome :as poly]
            [monome.kit.sampler :as samp]
            [cassiopeia.engine.monome-sequencer :as monome-sequencer]))

(defonce m128 (mon/find-monome "/dev/tty.usbserial-m0000965"))

(when m128
  (defonce ticker128-fon    (fon/mk-fonome ::ticker128 16 6))
  (defonce ticker128        (monome-sequencer/mk-ticker ticker128-fon ::ticker128))
  (defonce __dock_ticker__  (poly/dock-fonome! m128 ticker128-fon ::ticker128 0 0))
  )
