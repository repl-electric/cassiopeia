(ns cassiopeia.warm-up-monome
  (:require [monome.core :as mon]
            [monome.fonome :as fon]
            [monome.polynome :as poly]
            [monome.kit.sampler :as samp]
            [cassiopeia.engine.monome-sequencer :as monome-sequencer]))

(defonce m128 (mon/find-monome "/dev/tty.usbserial-m0000965"))

(when m128
  (defonce seq128-fon (fon/mk-fonome ::seq128 16 6))
  (defonce ticker (monome-sequencer/mk-ticker seq128-fon))
  )
