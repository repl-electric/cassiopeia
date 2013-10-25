(ns stars.warm-up
  (:use
   [overtone.live]
   [stars.synths.mixers :only [basic-mixer]]
   [nano-kontrol2.config :only [mixer-init-state basic-mixer-init-state]])
  (:require
   [nano-kontrol2.core :as nk2]
   [nano-kontrol2.buttons :as btn]
   [clojure.edn :as edn]

   [stars.engine.timing :as tim]
   [stars.engine.mixer :as mx]))

(defn nk-bank
  "Returns the nk bank number for the specified bank key"
  [bank-k]
  (case bank-k
    :master 0    ; record
    :m64    2    ; play
    :m128   4    ; stop
    :riffs  8    ; fast-forward
    :synths 16)) ; rewind

(defonce default-mixer-g (group :tail (foundation-safe-post-default-group)))

(def cfg
  {:synths {:s0 mixer-init-state :s1 mixer-init-state :s2 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r0 mixer-init-state :r7 basic-mixer-init-state}
   :riffs  {:s0 mixer-init-state :s1 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r7 basic-mixer-init-state}
   :master {:s7 mixer-init-state :m7 mixer-init-state :r7 mixer-init-state}})

(def banks
  {:master btn/record
   :m64    btn/play
   :m128   btn/stop
   :riffs  btn/fast-forward
   :synths btn/rewind})

(ctl tim/root-s :rate 5)

(nk2/start! banks cfg)

(defonce synth-bus (audio-bus 2))
(defonce riffs-bus (audio-bus 2))

(defonce mixer-s0 (mx/add-nk-mixer (nk-bank :synths) :s0 default-mixer-g synth-bus))
(defonce mixer-s1 (mx/add-nk-mixer (nk-bank :synths) :s1 default-mixer-g synth-bus))
(defonce mixer-m0 (mx/add-nk-mixer (nk-bank :synths) :m0 default-mixer-g synth-bus))
(defonce mixer-m1 (mx/add-nk-mixer (nk-bank :synths) :m1 default-mixer-g synth-bus))
(defonce mixer-s2 (mx/add-nk-mixer (nk-bank :synths) :s2 default-mixer-g synth-bus))
(defonce mixer-r0 (mx/add-nk-mixer (nk-bank :synths) :r0 default-mixer-g synth-bus))

(defonce basic-synth-mix (basic-mixer [:after default-mixer-g] :in-bus synth-bus))
(defonce basic-riffs-mix (basic-mixer [:after default-mixer-g] :in-bus riffs-bus))

(defonce mixer-riff-s0 (mx/add-nk-mixer (nk-bank :riffs) :s0 default-mixer-g riffs-bus))
(defonce mixer-riff-s1 (mx/add-nk-mixer (nk-bank :riffs) :s1 default-mixer-g riffs-bus))
(defonce mixer-riff-m0 (mx/add-nk-mixer (nk-bank :riffs) :m0 default-mixer-g riffs-bus))
(defonce mixer-riff-m1 (mx/add-nk-mixer (nk-bank :riffs) :m1 default-mixer-g riffs-bus))
(defonce mixer-riff-s2 (mx/add-nk-mixer (nk-bank :riffs) :s2 default-mixer-g riffs-bus))
(defonce mixer-riff-r0 (mx/add-nk-mixer (nk-bank :riffs) :r0 default-mixer-g riffs-bus))

(on-latest-event [:v-nanoKON2 (nk-bank :synths) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-synth-mix :amp val))
                 ::synths-master-amp)

(on-latest-event [:v-nanoKON2 (nk-bank :riffs) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-riffs-mix :amp val))
                 ::riffs-master-amp)
