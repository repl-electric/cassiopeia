(ns cassiopeia.destination.tsih
"
 _______ _______ _____ _     _
    |    |______   |   |_____|
    |    ______| __|__ |     |

 An eruptive variable star, whose brightness changes irregularly between +2.15 mag and +3.40 mag
"

  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.mixers :as mix]
            [overtone.inst.synth :as s])
  (:use [overtone.live]
        [cassiopeia.samples]
        [cassiopeia.destination.tsih-orchestra]))

(do
  (ctl time/root-s :rate 4)

  (defn buf-cycle! [buf list]
    (buffer-write! buf (take (buffer-size buf) (cycle (map #(if (keyword? %) (note %) %) list)))))

  (defonce power-kick-g (group "Powerish kick"))
  (defonce power-kick-seq (buffer 16))
  (defonce kick-buf (buffer 16))
  (defonce kick-seq-buf (buffer 16))
  (defonce kick2-g (group "kick2"))
  (defonce notes-buf (buffer 128))
  (defonce shrill-buf (buffer 128))
  (defonce growl-buf (buffer 128))
  (defonce growl-amp-buf (buffer 128))
  (defonce glass-g (group "glass"))
  (defonce mid-glass-g (group "A little more classey glass"))
  (defonce mid-ping-notes-buf (buffer 32))
  (defonce mid-ping-seq-buf (buffer 32))
  (defonce bass-notes-buf (buffer 32))
  (defonce ping-bass-seq-buf (buffer 32))
  (defonce phase-bass-buf (buffer 32))
  (defonce bazz-g (group "bazz group"))
  (defonce mid-ping-notes-buf (buffer 128))
  (defonce mid-ping-seq-buf  (buffer 18))
  (defonce white-seq-buf (buffer 24))
  (defonce white-notes-buf (buffer 24))
  (defonce white-g (group "whitenoise-hat"))
  (defonce shrill-seq-buf (buffer 32))
  (defonce shrill-dur-buf (buffer 32))
  (defonce fizzy-duration (buffer 128))
  (defonce shrill-pong-g (group "Shrill and flowery pong"))
  (defonce f-shrill-buf (buffer 128)))

(do
  (kill bazz-g)
  (kill power-kick-g)
  (kill kick2-g)

  (buf-cycle! phase-bass-buf [0 0 1 1 0 0
                              0 0 1 1 0 0
                              0 0 1 1 0 0
                              0 0 1 0 1 0 ])

  (doseq [i (range 0 32)]
    (bazz
     [:head bazz-g]
     :amp 0.7
     :mix (nth (take 32 (cycle [0.02 0.2])) i)
     :room 2
     ;;   :damp 0.6
     :note-buf bass-notes-buf
     :seq-buf phase-bass-buf
     :beat-bus     (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num i))
  (ctl bazz-g :damp 0.6)

  (doall (map-indexed
          #(quick-kick
            [:head power-kick-g]
            :amp 0.5
            :note-buf bass-notes-buf
            :seq-buf power-kick-seq
            :beat-bus (:count time/beat-1th)
            :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :beat-num %2) (range 0 16)))

  (buf-cycle! power-kick-seq [0 0 1  0 0 0
                              0 0 0  0 0 0
                              0 0 0  0 0 0
                              0 0 0  0 0 0])

  (doseq [i (range 0 32)]
    (kick2
     [:head kick2-g]
     :note-buf bass-notes-buf
     :seq-buf  kick-seq-buf
     :beat-bus     (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th)
     :num-steps 32
     :beat-num i))

  (ctl time/root-s :rate 4)
  (buf-cycle! kick-seq-buf [0 0 1 0 0 0
                            0 0 1 0 0 0
                            0 0 1 0 0 0
                            0 0 1 1 0 0])
  (buf-cycle! white-seq-buf [0]))

(ctl time/root-s :rate 0)

;;(mono-player moore-s :amp 1 :rate 1)
;;(echoey-buf :b moore-s)
(spacy moore-s)

(kill white-g)
(doseq [i (range 0 24)]
  (whitenoise-hat
   [:head white-g]
   :amp (+ 0.1 (/  i 12))
   :seq-buf  white-seq-buf
   :beat-bus     (:count time/beat-1th)
   :beat-trg-bus (:beat time/beat-1th)
   :num-steps 24
   :beat-num i))

(buf-cycle! white-seq-buf [1 0])
(buf-cycle! white-seq-buf [1])
(buf-cycle! white-seq-buf [0 0 0 1 1 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0])

(kill whitenoise-hat)
(kill kick2-g)
(kill bazz)

(buf-cycle! kick-seq-buf [0 0 1 0 0 1 0 0 1 1])

(doall (map-indexed #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18)))

(doall (map-indexed #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num (+ 2 %2)) (range 0 18)))

(do
  (def mid-pings (doall (map-indexed #(glass-ping [:head mid-glass-g] :amp 1 :note-buf mid-ping-notes-buf :seq-buf mid-ping-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

  (when (node-live? p) (ctl p :amp 0))
  (when (node-live? q) (ctl q :amp 0))

  (buf-cycle! growl-amp-buf  [1])
  (buf-cycle! growl-buf [:D3 :D3 :D3  :E3 :E3 :E3  :A4 :A4 :A4
                         :D4 :D4 :D4  :F#4 :F#4 :F#4])
  (kill glass-g))

(buf-cycle! ping-bass-seq-buf [1 0 1 0])

(buf-cycle! bass-notes-buf [:A2 :A2 :A4 :A5 :A2 :A6 :A5])
(buf-cycle! bass-notes-buf [:E2 :E2 :E4 :E5 :E2 :E5 :E4])
(buf-cycle! bass-notes-buf [:D2 :D2 :D5 :D6 :D2 :D6 :D5])

(buf-cycle! bass-notes-buf [:A4 :E4 :E2 :E2 :D5 :D4
                            :E4 :E4 :A2 :A2 :D5 :D4
                            :D5 :D6 :A2 :E2 :D6 :D5])

(buf-cycle! bass-notes-buf [:A1 :A1 :A1 :A1 :A1 :A1])

(buf-cycle! growl-amp-buf [1 1 0 1 1 0])
(buf-cycle! growl-buf [:D4 :D4 0 :A4 :A4 0])

(buf-cycle! mid-ping-seq-buf [1 1 0 0])
(buf-cycle! mid-ping-seq-buf [0])

(buf-cycle! mid-ping-notes-buf [:A4 :A4 :D4 :D4 :D4 :E4])
(buf-cycle! mid-ping-notes-buf [:A4 :A4 :D4 :D4 :D4 :E4
                                0 0 0 0 0 0
                                :A4 :A4 :D4 :D4 :D4 :F#4
                                0 0 0 0 0 0
                                :A4 :A4 :D4 :D4 :D4 :F#4
                                0 0 0 0 0 0])

(buf-cycle! phase-bass-buf [0 1])
(buf-cycle! phase-bass-buf [1 1 0 0 0 0 0 0])

(buf-cycle!  kick-seq-buf [1 0 0 0])

(def p (pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf notes-buf :amp 0.7))

(def q (shrill-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf shrill-buf :amp 0.7))

(def growl-synth (growl :amp 1.8 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th) :note-buf growl-buf :growl-amp-buf growl-amp-buf))

(def dark (dark-ambience :mul 0.4 :amp 0.4 :ring-freq (midi->hz (note :A3))))

;;:A5 :E5 :G#5
(ctl dark :ring-freq (midi->hz (note :A3)))

(s/rise-fall-pad :freq (midi->hz (note :A3)))

(buf-cycle! growl-buf [:D4 :D4 0 :A4 :A4 0])

(buf-cycle! growl-amp-buf [1 1 0 1 1 0 1 1])

(buf-cycle! growl-buf [:A3 :A3 0 :E3 :E3 0 :G#3 :G#3])
(buf-cycle! growl-buf [:D3 :D3 0 :E3 :E3])

(buf-cycle! growl-buf [:G3 :G3 0 :G3 :G3 0
                       :E3 :E3 0 :E3 :G3 0])

(buf-cycle! growl-buf [:E3 :E3 0 :E3 :E3 0
                       :A3 :A3 0 :A3 :A3 0])

(buf-cycle! growl-amp-buf [1 1 0 1 1])
(buf-cycle! growl-buf [:D3 :D3 0 :D3 :D3])

(ctl growl-synth :amp 1.8)

(buf-cycle! notes-buf [:A3 0 :A3])
(buf-cycle! notes-buf [:E3 0 :E3])
(buf-cycle! notes-buf [:D3 0 :D3])

(buf-cycle! shrill-buf [0 :A3 0 :A3 0])

(buf-cycle! notes-buf [0])
(buf-cycle! shrill-buf [0])

(def p (fizzy-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf notes-buf :duration-bus fizzy-duration))

(ctl growl-synth :amp 0)
(buf-cycle! growl-amp-buf [1])
(def g (growler :amp 1 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th) :note-buf growl-buf :growl-amp-buf growl-amp-buf))

(ctl time/root-s :rate 4)

(ctl g :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

(ctl growl-synth :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

(buf-cycle! growl-buf  [:E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3

                        :D3 :D3 :D3     :D3 :D3 :D3
                        :D3 :D3 :D3     :D3 :D3 :D3

                        :F#3 :F#3 :F#3  :F#3 :F#3 :F#3
                        :F#3 :F#3 :F#3  :F#3 :F#3 :F#3

                        :A3 :A3 :A3  :A3 :A3 :A3
                        :A3 :A3 :A3  :A3 :A3 :A3])

(buf-cycle! fizzy-duration [1 1/2 1/2])
(buf-cycle! notes-buf  [0  0   :D4 0 0 :D4
                        0  0   :D4 0 0 :D4
                        0  0   :E4 0 0 :E4
                       :E4 :E4 :E4 0 0 :E4])

(buf-cycle! f-shrill-buf [:G#4 :E4 :D4  :G#4 :E4 :D4
                          :G#4 :E4 :D4  :G#4 :E4 :A4

                          :A4 :E4 :G#4  :A4 :E4 :G#4
                          :A4 :E4 :G#4  :A4 :E4 :G#4

                          :A5 :E5 :G#5  :A5 :E5 :G#5
                          :A5 :E5 :G#5  :A5 :E5 :G#5

                          :G#4 :E4 :D4  :G#4 :E4 :D4
                          :G#4 :E4 :D4  :G#4 :E4 :A4

                          :A4 :E4 :G#4  :A4 :E4 :G#4
                          :A4 :E4 :G#4  :A4 :E4 :G#4
                          :A4 :E4 :G#4  :A4 :E4 :G#4
                          :A4 :E4 :G#4  :A4 :E4 :G#4

                          :B5 :E5 :G#5  :A5 :E5 :G#5
                          :B5 :E5 :G#5  :A5 :E5 :G#5])

(buf-cycle! growl-buf [:G#2 :G#2 :G#2 :G#2 :G#2 :G#2
                       :A3  :A3  :A3  :A3  :A3  :A3])

(buf-cycle! f-shrill-buf [:G#3 :E3 :D3  :G#3 :E3 :D3
                          :G#3 :E3 :D3  :G#3 :E3 :A4
                          :G#3 :E3 :D3])

(buf-cycle! kick-seq-buf [1 0 0 1 0 0
                          1 0 0 1 0 0
                          1 0 0 1 0 0
                          1 1 0 1 0 1])

(ctl glass-g :amp 0)
(ctl mid-glass-g :amp 0)
(kill glass-g)
(kill mid-glass-g)

(shrill-pong [:head shrill-pong-g] :amp 1 :note-buf shrill-buf :duration-bus shrill-dur-buf :seq-buf shrill-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

;;(kill bazz-g)

(buf-cycle! white-seq-buf [0 0 0 1 1 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0])

(ctl white-g :amp 0.4)

(ctl shrill-pong-g :note-buf f-shrill-buf)

(buf-cycle! shrill-seq-buf [1])
(buf-cycle! shrill-dur-buf [1/12])

(buf-cycle! shrill-dur-buf [1/8 1/16 1/16 1/8 1/16 1/16])
(buf-cycle! shrill-dur-buf [1/4 1/8 1/8 1/4 1/8 1/8])
(buf-cycle! shrill-dur-buf [1/2 1/4 1/4 1/2 1/4 1/4])

(kill shrill-pong-g)

(buf-cycle! notes-buf [0])

(comment
  (def fx2 (fx/fx-chorus 0))
  (def fx3 (fx/fx-echo 0))

  (kill fx2)
  (kill fx3)
  (kill melody)
  (kill whitenoise-hat)
  (kill kick2-g)
  (kill bazz)
  (kill power-kick-g)
  (kill kick2)
  (kill mid-pings)
  (kill shrill-pulsar)
  (kill fizzy-pulsar)
  (kill pulsar)
  (kill glass-ping)
  (kill bazz)
  (kill growl)
  (kill growler)
  (kill dark-ambience)

  (stop)
)
