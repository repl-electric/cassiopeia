(ns stars.engine.timing
  (:use [overtone.core])
  (:require
   [overtone.synth.timing :as timing]))

;;(defonce root-b (control-bus))

(defonce count-trig-id (trig-id))
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count
(defonce pi-x-bus (control-bus))

(def BEAT-FRACTION "Number of global pulses per beat" 30)
(def current-beat (atom BEAT-FRACTION))
(defonce r-cnt (timing/counter :in-bus root-trg-bus :out-bus root-cnt-bus))
(defonce r-trg (timing/trigger :rate 100 :in-bus root-trg-bus))
(defonce b-cnt (timing/counter :in-bus beat-trg-bus :out-bus beat-cnt-bus))
(defonce b-trg (timing/divider :div BEAT-FRACTION :in-bus root-trg-bus :out-bus beat-trg-bus))
(defsynth get-beat [] (send-trig (in:kr beat-trg-bus) count-trig-id (+ (in:kr beat-cnt-bus) 1)))

(defonce get-beat-s (get-beat))
