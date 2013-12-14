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
   [launchpad.plugin.beat-scroll :as beat-scroll]

   [launchpad.plugin.sample-rows :as sr]

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
    :master btn/record
    :lp64   btn/play
    ;;      btn/stop    ; stop
    :riffs  btn/fast-forward ; fast-forward
    :synths btn/rewind)) ; rewind

(def cfg
  {:synths {:s0 mixer-init-state :s1 mixer-init-state :s2 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r0 mixer-init-state :r7 basic-mixer-init-state}
   :riffs  {:s0 mixer-init-state :s1 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r7 basic-mixer-init-state}
   :master {:s7 mixer-init-state :m7 mixer-init-state :r7 mixer-init-state}
   :lp64 {
          ;;Beats
          :s0 ["lp64-0" mixer-init-state]
          :m0 ["lp64-1" mixer-init-state]
          :r0 ["lp64-2" mixer-init-state]
          :s1 ["lp64-3" mixer-init-state]
          :m1 ["lp64-4" mixer-init-state]
          :r1 ["lp64-5" mixer-init-state]
          :s2 ["lp64-6" mixer-init-state]

          :s3 ["lp64-triggers" mixer-init-state]
          :r7 ["lp64-master" basic-mixer-init-state]

          ;;Row mapped samples
          :s4 ["lp64-seq-0" mixer-init-state]
          :m4 ["lp64-seq-1" mixer-init-state]
          :r4 ["lp64-seq-2" mixer-init-state]
          :s5 ["lp64-seq-3" mixer-init-state]
          :m5 ["lp64-seq-4" mixer-init-state]
          :r5 ["lp64-seq-5" mixer-init-state]
          :s6 ["lp64-seq-6" mixer-init-state]
          :m6 ["lp64-seq-7" mixer-init-state]}})

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

;;(def samples-set-1 [kick-s snare-s shaker-s hat-s])
(def samples-set-1 [custom-kick-s custom-long-shake-s custom-shake-s custom-tom-shake-s custom-fast-shake-s])

(defonce default-mixer-g (group :tail (foundation-safe-post-default-group)))
(defonce drum-g (group))
(defonce drum-trigger-mix-g (group :after drum-g))
(defonce drum-basic-mixer-g (group :after default-mixer-g))

(defonce lp64-b  (audio-bus 2 "lp64 basic-mixer"))
(defonce bas-mix-s64  (mixers/basic-mixer [:head drum-basic-mixer-g] :in-bus lp64-b :mute 0))
(defonce trig64-mixer (mixers/add-nk-mixer (nk-bank :lp64) "lp64-triggers" drum-trigger-mix-g lp64-b))

(ctl bas-mix-s64 :mute 1)

(defonce sequencer-64
  (sequencer/mk-sequencer
   (nk-bank :lp64)
   "lp64"
   samples-set-1
   phrase-size
   drum-g
   timing/beat-cnt-bus
   timing/beat-bus
   lp64-b))

(defonce refresh-beat-key (uuid))

(on-trigger timing/count-trig-id (beat-scroll/grid-refresh lp sequencer-64 phrase-size :up) refresh-beat-key)
(beat/setup-side-controls :up sequencer-64)

;;Adjust bpm
(lp-core/bind :up :7x6 (fn [] (ctl timing/divider-s :div (swap! timing/current-beat inc))))
(lp-core/bind :up :7x5 (fn [] (ctl timing/divider-s :div (swap! timing/current-beat dec))))

;;Shutdown
(lp-core/bind :up :arm  (fn [lp]
                          (ctl bas-mix-s64 :mute 0)
                          (beat/off lp sequencer-64)))


(ctl bas-mix-s64 :mute 1)

(defonce seq-g (group))
(def seq-mixer-group (group "lp-mixers" :after seq-g))
(defonce seq-trigger-mix-g (group :after seq-g))
(defonce seq-basic-mixer-g (group :after default-mixer-g))
(defonce seq-mix-s64  (mixers/basic-mixer [:head seq-basic-mixer-g] :in-bus lp64-b :mute 0))

(def sample-selection [arp-s
                       arp-chord-s
                       voice-1-s
                       voice-2-s
                       strings-s
                       drums-s
                       chords-s
                       dub-s

                       bass-1-s
                       bass-2-s
                       bass-3-s
                       hard-1-s
                       hard-2-s
                       hard-3-s
                       gtr-1-s

                       gtr-2-s
                       gtr-3-s
                       gtr-str-s])

(def seq-mixers  (doall (map-indexed (fn [idx _] (mixers/add-nk-mixer (nk-bank :lp64) (str "lp64-seq-" idx) seq-mixer-group lp64-b)) sample-selection)))

(def all-row-samples
  (doall (map-indexed
          (fn [idx sample] {:row idx
                           :sample sample
                           :sequencer (lp-sequencer/phasor-skipping-sequencer [:tail seq-g]
                                                                              :buf (to-sc-id sample)
                                                                              :loop? true
                                                                              :bar-trg 0
                                                                              :amp 0
                                                                              :out-bus (:in-bus (get-in seq-mixers [idx] {:in-bus 0})))})
          sample-selection)))

(use 'launchpad.plugin.sample-rows :reload)
(sample-rows lp :left all-row-samples)

(defonce synth-bus (audio-bus 2))
(defonce riffs-bus (audio-bus 2))

(defonce mixer-s0 (mixers/add-nk-mixer (nk-bank :synths) :s0 default-mixer-g synth-bus))
(defonce mixer-s1 (mixers/add-nk-mixer (nk-bank :synths) :s1 default-mixer-g synth-bus))
(defonce mixer-m0 (mixers/add-nk-mixer (nk-bank :synths) :m0 default-mixer-g synth-bus))
(defonce mixer-m1 (mixers/add-nk-mixer (nk-bank :synths) :m1 default-mixer-g synth-bus))
(defonce mixer-s2 (mixers/add-nk-mixer (nk-bank :synths) :s2 default-mixer-g synth-bus))
(defonce mixer-r0 (mixers/add-nk-mixer (nk-bank :synths) :r0 default-mixer-g synth-bus))

(defonce basic-synth-mix (mixers/basic-mixer [:after default-mixer-g] :in-bus synth-bus))
(defonce basic-riffs-mix (mixers/basic-mixer [:after default-mixer-g] :in-bus riffs-bus))

(defonce mixer-riff-s0 (mixers/add-nk-mixer (nk-bank :riffs) :s0 default-mixer-g riffs-bus))
(defonce mixer-riff-s1 (mixers/add-nk-mixer (nk-bank :riffs) :s1 default-mixer-g riffs-bus))
(defonce mixer-riff-m0 (mixers/add-nk-mixer (nk-bank :riffs) :m0 default-mixer-g riffs-bus))
(defonce mixer-riff-m1 (mixers/add-nk-mixer (nk-bank :riffs) :m1 default-mixer-g riffs-bus))
(defonce mixer-riff-s2 (mixers/add-nk-mixer (nk-bank :riffs) :s2 default-mixer-g riffs-bus))
(defonce mixer-riff-r0 (mixers/add-nk-mixer (nk-bank :riffs) :r0 default-mixer-g riffs-bus))

(on-latest-event [:v-nanoKON2 (nk-bank :synths) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-synth-mix :amp val))
                 ::synths-master-amp)

(on-latest-event [:v-nanoKON2 (nk-bank :riffs) :r7 :control-change :slider7]
                 (fn [{:keys [val]}]
                   (ctl basic-riffs-mix :amp val))
                 ::riffs-master-amp)
