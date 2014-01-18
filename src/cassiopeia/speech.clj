(ns cassiopeia.speech
  (:use [overtone.live]))

(def m1 ((speech-buffer "have you ever transcended space and time?" :voice :victoria) :rate 1 :loop? true :out-bus 0))
(def m2 ((speech-buffer "have you ever transcended space and time?" :voice :princess) :rate 1 :loop? true :out-bus 0))
(def m3 ((speech-buffer "have you ever transcended space and time?" :voice :kathy) :rate 1 :loop? true :out-bus 0))
(def m4 ((speech-buffer "have you ever transcended space and time?" :voice :vicki) :rate 1 :loop? true :out-bus 0))

(ctl m1 :rate 0.95)
(ctl m2 :rate 0.85)
(ctl m3 :rate 0.75)
(ctl m4 :rate 0.65)

(ctl m1 :rate 0.85 :amp 0.2)
(ctl m2 :rate 0.75 :amp 0.2)
(ctl m3 :rate 0.65 :amp 0.2)
(ctl m4 :rate 0.55 :amp 0.2)

(stop)
