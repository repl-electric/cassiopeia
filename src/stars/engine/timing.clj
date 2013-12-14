(ns stars.engine.timing
  (:use [overtone.live])
  (:require
   [overtone.synth.timing :as timing]))

(defonce timing-g (group "stars timing" :tgt (foundation-safe-pre-default-group)))

(defonce root-b (control-bus))
(defonce x-b          (control-bus))
(defonce inv-root-b   (control-bus))
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-bus (control-bus))     ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

(defonce count-trig-id (trig-id))

(def BEAT-FRACTION "Number of global pulses per beat" 30)
(def current-beat (atom BEAT-FRACTION))

(defsynth get-beat [] (send-trig (in:kr beat-bus) count-trig-id (+ (in:kr beat-cnt-bus) 1)))

(defsynth root-saw [rate 10 saw-bus 0 inv-saw-bus 0]
  (let [saw (lf-saw:kr rate)]
    (out:kr inv-saw-bus (* -1 saw))
    (out:kr saw-bus saw)))

(defsynth buf-phasor [saw-x-b 0 out-bus 0 buf 0]
  (let [n-samps (buf-frames buf)
        phase   (* n-samps (in:kr saw-x-b))]
    (out:kr out-bus (buf-rd:kr 1 buf phase :loop 0 :interpolation 1))))

(defsynth saw-x [out-bus 0 freq-mul 1 mul 1 add 0 smoothness 0 x-bus (:id x-b)]
  (out:kr out-bus (lag (mul-add (mod (* freq-mul (in:kr x-bus)) 1) mul add))))

(defonce root-s    (root-saw [:head timing-g] :saw-bus root-b :inv-saw-bus inv-root-b :rate 2))
(defonce r-cnt (timing/counter :in-bus root-trg-bus :out-bus root-cnt-bus))
(defonce r-trg (timing/trigger :rate 100 :in-bus root-trg-bus))
(defonce divider-s (timing/divider [:after root-s] :div 1 :in-bus inv-root-b :out-bus beat-bus))
(defonce b-cnt     (timing/counter [:after divider-s] :in-bus beat-bus :out-bus beat-cnt-bus))

(defonce get-beat-s (get-beat [:after divider-s]))
