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
   [launchpad.sequencer :as sequencer]
   [nano-kontrol2.core :as nk2]
   [nano-kontrol2.buttons :as btn]
   [clojure.edn :as edn]
   [stars.engine.timing :as timing]))

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

(lp-core/boot!)
(nk2/start! banks cfg)

(def lp (first lp-core/launchpad-kons))
(def phrase-size 16)

(defonce beat-rep-key (uuid))
(metronome/start lp :mixer timing/count-trig-id beat-rep-key)

(def samples-set-1 [kick-s snare-s high-hat-open-s heavy-bass-kick-s clap-s sizzling-high-hat-s])

(def lp-sequencer (sequencer/mk-sequencer "launchpad-sequencer" samples-set-1 phrase-size timing/beat-cnt-bus timing/beat-trg-bus 0))

(defonce refresh-beat-key (uuid))

(on-trigger timing/count-trig-id (beat/grid-refresh lp lp-sequencer phrase-size) refresh-beat-key)
(beat/setup-side-controls :up lp-sequencer)

;;Adjust bpm
(lp-core/bind :up :7x6 (fn [] (ctl timing/b-trg :div (swap! timing/current-beat inc))))
(lp-core/bind :up :7x5 (fn [] (ctl timing/b-trg :div (swap! timing/current-beat dec))))

;;Shutdown
(lp-core/bind :up :arm  (fn [lp] (beat/off lp lp-sequencer)))
