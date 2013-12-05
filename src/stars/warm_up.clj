(ns stars.warm-up
  (:use
   [overtone.live]
   [overtone.helpers.lib :only [uuid]]
   [nano-kontrol2.config :only [mixer-init-state basic-mixer-init-state]]
   [stars.samples])
  (:require
   [launchpad.core :as lp-core]
   [launchpad.plugin.metronome :as metronome]
   [launchpad.plugin.beat :as beat]
   [launchpad.sequencer :as lp-sequencer]
   [nano-kontrol2.core :as nk2]
   [nano-kontrol2.buttons :as btn]

   [stars.engine.timing :as timing]
   [stars.engine.sequencer :as sequencer]
   [stars.engine.mixers :as mixers]))

(defn nk-bank
  "Returns the nk bank number for the specified bank key"
  [bank-k]
  (case bank-k
    :master 0    ; record
    :lp64   2    ; play
;;          4    ; stop
    :riffs  8    ; fast-forward
    :synths 16)) ; rewind

(defonce default-mixer-g (group :tail (foundation-safe-post-default-group)))

(def cfg
  {:synths {:s0 mixer-init-state :s1 mixer-init-state :s2 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r0 mixer-init-state :r7 basic-mixer-init-state}
   :riffs  {:s0 mixer-init-state :s1 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r7 basic-mixer-init-state}
   :master {:s7 mixer-init-state :m7 mixer-init-state :r7 mixer-init-state}
   :lp64 {:s0 mixer-init-state :s1 mixer-init-state :s2 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r0 mixer-init-state :r7 basic-mixer-init-state}}
  )

(def banks
  {:master btn/record
   :lp64   btn/play
   :riffs  btn/fast-forward
   :synths btn/rewind})

(lp-core/boot!)
(nk2/start! banks cfg)

(def lp (first lp-core/launchpad-kons))
(def phrase-size 16)

(defonce beat-rep-key (uuid))
(metronome/start lp :mixer timing/count-trig-id beat-rep-key)

(def samples-set-1 [kick-s snare-s high-hat-open-s heavy-bass-kick-s clap-s sizzling-high-hat-s])

(defonce drum-g             (group))
;;(defonce drum-trigger-mix-g (group :after drum-g))
;;(defonce drum-basic-mixer-g (group :after default-mixer-g))

(def sequencer-64
  (sequencer/mk-sequencer
   (nk-bank :lp64)
   "lp68"
   samples-set-1
   phrase-size
   drum-g
   timing/beat-cnt-bus
   timing/beat-trg-bus
   0))

;;(def sequencer-64 (lp-sequencer/mk-sequencer "launchpad-sequencer" samples-set-1 phrase-size timing/beat-cnt-bus timing/beat-trg-bus 0))

(defonce refresh-beat-key (uuid))

(on-trigger timing/count-trig-id (beat/grid-refresh lp sequencer-64 phrase-size) refresh-beat-key)
(beat/setup-side-controls :up sequencer-64)

;;Adjust bpm
(lp-core/bind :up :7x6 (fn [] (ctl timing/b-trg :div (swap! timing/current-beat inc))))
(lp-core/bind :up :7x5 (fn [] (ctl timing/b-trg :div (swap! timing/current-beat dec))))

;;Shutdown
(lp-core/bind :up :arm  (fn [lp] (beat/off lp sequencer-64)))


(defonce synth-bus (audio-bus 2))
(defonce riffs-bus (audio-bus 2))
(defonce basic-synth-mix (mixers/basic-mixer [:after default-mixer-g] :in-bus synth-bus))
(defonce basic-riffs-mix (mixers/basic-mixer [:after default-mixer-g] :in-bus riffs-bus))

(on-latest-event [:v-nanoKON2 (nk-bank :synths) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-synth-mix :amp val))
                 ::synths-master-amp)

(on-latest-event [:v-nanoKON2 (nk-bank :riffs) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-riffs-mix :amp val))
                 ::riffs-master-amp)
