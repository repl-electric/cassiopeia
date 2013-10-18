(ns stars.timing
  (:use [overtone.core]
        [overtone.synth.timing]))

(defonce timing-g (group "M-x timing" :tgt (foundation-safe-pre-default-group)))

(defonce root-b       (control-bus))
(defonce inv-root-b   (control-bus))
(defonce offset-b     (control-bus))
(defonce pi-offset-b  (control-bus))
(defonce count-b      (control-bus))
(defonce pi-count-b   (control-bus))
(defonce x-b          (control-bus))
(defonce pi-x-b       (control-bus))
(defonce sin-b        (control-bus))
(defonce x-mul-b      (control-bus))
(defonce beat-b       (control-bus))
(defonce beat-count-b (control-bus))

