(ns cassiopeia.warm-up
  (:use
   [overtone.live]
   [overtone.helpers.lib :only [uuid]]
   [nano-kontrol2.config :only [mixer-init-state basic-mixer-init-state]]
   [cassiopeia.samples])
  (:require
   [launchpad.core :as lp-core]
   [launchpad.plugin.metronome :as metronome]
   [launchpad.plugin.beat :as beat]
   [launchpad.plugin.beat-scroll :as beat-scroll]

   [launchpad.plugin.sample-rows :as sr]

   [launchpad.sequencer :as lp-sequencer]

   [nano-kontrol2.core :as nk2]
   [nano-kontrol2.buttons :as btn]

   [monome.core :as mon]
   [monome.fonome :as fon]
   [monome.polynome :as poly]
   [monome.kit.sampler :as samp]
   [cassiopeia.engine.monome-sequencer :as monome-sequencer]

   [cassiopeia.engine.timing :as timing]
   [cassiopeia.engine.sequencer :as sequencer]
   [cassiopeia.engine.mixers :as mixers]))

(defn nk-bank
  "Returns the nk bank number for the specified bank key"
  [bank-k]
  (case bank-k
    :master btn/record
    :lp64   btn/play
    :m128   btn/stop
    :riffs  btn/fast-forward
    :synths btn/rewind))

(def cfg
  {:synths {:s0 mixer-init-state :s1 mixer-init-state :s2 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r0 mixer-init-state :r7 basic-mixer-init-state}
   :riffs  {:s0 mixer-init-state :s1 mixer-init-state :m0 mixer-init-state :m1 mixer-init-state :r7 basic-mixer-init-state}
   :master {:s7 mixer-init-state :m7 mixer-init-state :r7 mixer-init-state}
   :m128 {
          :s0 ["m128-0" mixer-init-state]
          :m0 ["m128-1" mixer-init-state]
          :r0 ["m128-2" mixer-init-state]
          :s1 ["m128-3" mixer-init-state]
          :m1 ["m128-4" mixer-init-state]
          :r1 ["m128-5" mixer-init-state]
          :s2 ["m128-6" mixer-init-state]
          :r2 ["m128-7" mixer-init-state]
          :m2 ["m128-8" mixer-init-state]
          :s3 ["m128-9" mixer-init-state]
          }
   :lp64 {
          ;;Beats
          :s0 ["lp64-0" mixer-init-state]
          :m0 ["lp64-1" mixer-init-state]
          :r0 ["lp64-2" mixer-init-state]
          :s1 ["lp64-3" mixer-init-state]
          :m1 ["lp64-4" mixer-init-state]
          :r1 ["lp64-5" mixer-init-state]
          :s2 ["lp64-6" mixer-init-state]
          :r2 ["lp64-7" mixer-init-state]
          :m2 ["lp64-8" mixer-init-state]
          :s3 ["lp64-9" mixer-init-state]

          :m3 ["lp64-triggers" mixer-init-state]
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
   :m128   btn/stop
   :riffs  btn/fast-forward
   :synths btn/rewind})

(try
  (lp-core/boot!)
  (catch Exception e
    (println "LP not connected")))

(try
  (nk2/start! banks cfg)
  (def lp (first lp-core/launchpad-kons))
  (catch Exception e
    (def lp [])
    (println "NK2 not connected")))

(def phrase-size 8)

(defonce default-mixer-g (group :tail (foundation-safe-post-default-group)))
(defonce drum-g (group))
(defonce drum-trigger-mix-g (group :after drum-g))
(defonce drum-basic-mixer-g (group :after default-mixer-g))
(def samples-set-1 (take 10 (cycle [tom-s])))

(when-not (seq lp)
  (defonce seq-b  (audio-bus 2 "basic-mixer"))
  (defonce bas-mix-seq    (mixers/basic-mixer [:head drum-basic-mixer-g] :in-bus seq-b :mute 0))
  (defonce trig-seq-mixer (mixers/add-nk-mixer (nk-bank :lp64) "lp64-triggers" drum-trigger-mix-g seq-b))
;; (ctl bas-mix-seq :mute 1)

  (defonce sequencer-64
    (sequencer/mk-sequencer
     (nk-bank :lp64)
     "seq"
     samples-set-1
     phrase-size
     drum-g
     (atom timing/main-beat)
     seq-b)))

(when (seq lp)
  (defonce beat-rep-key (uuid))
  (metronome/start lp :mixer timing/count-trig-id beat-rep-key)

  (defonce lp64-b  (audio-bus 2 "lp64 basic-mixer"))
  (defonce bas-mix-s64  (mixers/basic-mixer [:head drum-basic-mixer-g] :in-bus lp64-b :mute 0))
  (defonce trig64-mixer (mixers/add-nk-mixer (nk-bank :lp64) "lp64-triggers" drum-trigger-mix-g lp64-b))

;;  (ctl bas-mix-s64 :mute 1)

  (defonce sequencer-64
    (sequencer/mk-sequencer
     (nk-bank :lp64)
     "lp64"
     samples-set-1
     phrase-size
     drum-g
     (atom timing/main-beat)
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


;;  (ctl bas-mix-s64 :mute 1)

  (defonce seq-g (group))
  (def seq-mixer-group (group "lp-mixers" :after seq-g))
  (defonce seq-trigger-mix-g (group :after seq-g))
  (defonce seq-basic-mixer-g (group :after default-mixer-g))
  (defonce seq-mix-s64  (mixers/basic-mixer [:head seq-basic-mixer-g] :in-bus lp64-b :mute 0))

;;  (ctl seq-mix-s64 :mute 1)

  (def sample-selection [])

  (defonce seq-mixers  (vec (doall (map-indexed (fn [idx _] (mixers/add-nk-mixer (nk-bank :lp64) (str "lp64-seq-" idx) seq-mixer-group lp64-b)) sample-selection))))
  ;;(def seq-mixers [])

  (get-in seq-mixers [1])

  (defonce rate-b  (control-bus 1 "Rate"))
  (defonce rater-s (sequencer/rater :out-bus rate-b))

  (def all-row-samples
    (doall (map-indexed
            (fn [idx sample] {:row idx
                             :sample sample
                             :sequencer (sequencer/phasor-skipping-sequencer [:tail seq-g]
                                                                             :buf (to-sc-id sample)
                                                                             :loop? true
                                                                             :bar-trg 0
                                                                             :amp 0
                                                                             :beat-b timing/beat-b
                                                                             :rate-b rate-b
                                                                             :out-bus (:in-bus (get-in seq-mixers [idx] {:in-bus 0})))})
            sample-selection)))

  (sr/sample-rows lp :left all-row-samples))

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

(def m128 (mon/find-monome "/dev/tty.usbserial-m0000965"))

(when m128
  (defonce seq128-fon (fon/mk-fonome ::seq128 16 6))
  (defonce insta-pause128-fon  (fon/mk-fonome ::pauser128 1 1))
  (defonce insta-pause-all-fon (fon/mk-fonome ::pauser-all 1 1))

  (defonce seq-b  (audio-bus 2 "basic-mixer"))
  (defonce bas-mix-seq    (mixers/basic-mixer [:head drum-basic-mixer-g] :in-bus seq-b :mute 0))
  (defonce trig-seq-mixer (mixers/add-nk-mixer (nk-bank :m128) "m128-triggers" drum-trigger-mix-g seq-b))
  (defonce seq128 (monome-sequencer/mk-monome-sequencer (nk-bank :m128) "m128" samples-set-1 seq128-fon seq-b drum-g))

  (def samples-g (group "samples"))
  (def trigger-samples [star-into-the-sun-s space-and-time-s chaos-s dreamers-of-the-dreams-s one-moment-please-s afraid-s glitch1-s glitch2-s pulse-s boom-s])
  (defonce trigger-sampler128  (samp/mk-sampler ::trigger-sampler128 trigger-samples samples-g 0 16))

  (defonce __dock_trigger__  (poly/dock-fonome! m128 (:fonome trigger-sampler128)
                                                ::trigger-sampler128 0 6))
  (defonce __dock128___ (poly/dock-fonome! m128 seq128-fon ::seq128 0 0))
  (defonce __dock_pause128__ (poly/dock-fonome! m128 insta-pause128-fon ::pause128 15 7))

  (on-event [:fonome :led-change (:id insta-pause128-fon)]
            (fn [{:keys [x y new-leds]}]
              (let [on? (get new-leds [x y])]
                (if on?
                  (ctl bas-mix-seq :mute 1)
                  (ctl bas-mix-seq :mute 0))))
            ::seq128)

  (on-event [:fonome :press (:id insta-pause128-fon)]
            (fn [{:keys [x y fonome]}]
              (fon/toggle-led fonome x y))
            ::seq128-press))

(comment
  (monome-sequencer/swap-samples! seq128 trigger-samples)
  )
