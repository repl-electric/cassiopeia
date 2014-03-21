(ns cassiopeia.destination.tsih
"
 _______ _______ _____ _     _
    |    |______   |   |_____|
    |    ______| __|__ |     |

 An eruptive variable star, whose brightness changes irregularly between
 +2.15 mag and +3.40 mag
"

  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.mixers :as mix]
            [overtone.inst.synth :as s]
            [shadertone.tone :as t])
  (:use [overtone.live]
        [cassiopeia.samples]
        [cassiopeia.view-screen]
        [cassiopeia.engine.synths]))
(stop)
(do
  (ctl time/root-s :rate 4)

  (defn buf-cycle! [buf list]
    (buffer-write! buf (take (buffer-size buf) (cycle (map #(if (keyword? %) (note %) %) list)))))

  (declare p q growl-synth)

  (defonce voice-g (group "main voice"))
  (defonce bass-g (group "bass voice"))
  (defonce drums-g (group "drums"))

  (defonce power-kick-seq (buffer 16))
  (defonce kick-buf (buffer 16))
  (defonce kick-seq-buf (buffer 16))
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
  (defonce mid-ping-notes-buf (buffer 128))
  (defonce mid-ping-seq-buf  (buffer 18))
  (defonce white-seq-buf (buffer 24))
  (defonce white-notes-buf (buffer 24))
  (defonce shrill-seq-buf (buffer 32))
  (defonce shrill-dur-buf (buffer 32))
  (defonce fizzy-duration (buffer 128))
  (defonce shrill-pong-buf (buffer 128))
  (defonce shrill-pong-final-buf (buffer 128)))

(do
  (kill high-hats)
  (kill quick-kick)
  (kill kick2)

  (reset! color-l 0.9)
  (reset! color-r 0.9)

  (buf-cycle! phase-bass-buf [0 0 1 1 0 0
                              0 0 1 1 0 0
                              0 0 1 1 0 0
                              0 0 1 0 1 0])

  (buf-cycle! bass-notes-buf [:A1])

  (def hats
    (doall (map #(high-hats
                  [:head drums-g]
                  :amp 0.7
                  :mix (nth (take 32 (cycle [0.02 0.2])) %1)
                  :room 2
                  ;;:damp 0.6
                  :note-buf bass-notes-buf
                  :seq-buf phase-bass-buf
                  :beat-bus     (:count time/beat-1th)
                  :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num %1) (range 0 32))))
  (ctl hats :damp 0.6)

  (def qk
    (doall (map
            #(quick-kick
              [:head drums-g]
              :amp 0.5
              :note-buf bass-notes-buf
              :seq-buf power-kick-seq
              :beat-bus (:count time/beat-1th)
              :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :beat-num %1) (range 0 16))))

  (buf-cycle! power-kick-seq [0 0 1  0 0 0
                              0 0 0  0 0 0
                              0 0 0  0 0 0
                              0 0 0  0 0 0])

  (doseq [i (range 0 32)]
    (kick2
     [:head drums-g]
     :note-buf bass-notes-buf
     :seq-buf  kick-seq-buf
     :beat-bus     (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th)
     :num-steps 32
     :beat-num i))

  (def s (shrill-pong [:head voice-g] :amp 1.1 :note-buf shrill-pong-buf :duration-bus shrill-dur-buf :seq-buf shrill-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))

  (ctl time/root-s :rate 4)
  (buf-cycle! kick-seq-buf [0 0 1 0 0 0
                            0 0 1 0 0 0
                            0 0 1 0 0 0
                            0 1 1 1 0 0])
  (buf-cycle! white-seq-buf [0]))

(reset! res 0.9)
(ctl time/root-s :rate 0)

(kill whitenoise-hat)
(def white (doall (map
                   #(whitenoise-hat
                    [:head drums-g]
                    :amp (+ 0.1 (/  %1 10))
                    :seq-buf  white-seq-buf
                    :beat-bus     (:count time/beat-1th)
                    :beat-trg-bus (:beat time/beat-1th)
                    :num-steps 24
                    :beat-num %1) (range 0 24))))


(buf-cycle! white-seq-buf [])
(buf-cycle! white-seq-buf [0 0 0 1 1 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0])

(kill whitenoise-hat)
(kill kick2)
(kill high-hats)

(buf-cycle! kick-seq-buf [0 0 1 0 0 1 0 0 1 1])

(doall (map #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %1) (range 0 18)))

(doall (map #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num (+ 2 %1)) (range 0 18)))

(do
  (def mid-pings (doall (map-indexed #(glass-ping [:head mid-glass-g] :amp 1 :note-buf mid-ping-notes-buf :seq-buf mid-ping-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

  (when (node-live? p) (ctl p :amp 0))
  (when (node-live? q) (ctl q :amp 0))
  ;;(when (node-live? q) (ctl s :amp 0))

  (buf-cycle! mid-ping-seq-buf [0])

  (buf-cycle! growl-amp-buf  [1])
  (buf-cycle! growl-buf [:D3 :D3 :D3  :E3 :E3 :E3  :A4 :A4 :A4
                         :C#4 :C#4 :C#4  :F#4 :F#4 :F#4])
  (kill glass-g))

(buf-cycle! ping-bass-seq-buf [1 1 0 0])

(buf-cycle! bass-notes-buf [:A2 :A2 :A4 :A5 :A2 :A6 :A5])
(buf-cycle! bass-notes-buf [:E2 :E2 :E4 :E5 :E2 :E5 :E4])
(buf-cycle! bass-notes-buf [:C#2 :C#2 :C#5 :C#6 :C#2 :C#6 :C#5])
(buf-cycle! bass-notes-buf [:B2 :B2 :B5 :B6 :B2 :B6 :B5])

;;(buf-cycle! bass-notes-buf [:D2 :D2 :D5 :D6 :D2 :D6 :D5])

(buf-cycle! bass-notes-buf [:A4 :E4 :E2 :E2 :D5 :D4
                            :E4 :E4 :A2 :A2 :D5 :D4
                            :C#5 :C#6 :A2 :E2 :C#6 :C#5])

(buf-cycle! bass-notes-buf [:A1 :A1 :A1 :A1 :A1 :A1])

(buf-cycle! growl-amp-buf [1 1 0 1 1 0])
(buf-cycle! growl-buf [:D4 :D4 0 :A4 :A4 0])

(buf-cycle! kick-seq-buf [1 1 0 0])
(buf-cycle! mid-ping-seq-buf [1 1 0 0])
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

(def growl-synth (growl [:head bass-g] :amp 0 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th) :note-buf growl-buf :growl-amp-buf growl-amp-buf))

(reset! color-r 0.9)
(def dark (dark-ambience :mul 0.4 :amp 0.4 :ring-freq (midi->hz (note :A3))))

(ctl dark :ring-freq (midi->hz (note :A3)))

(s/rise-fall-pad :freq (midi->hz (note :A3)))

(buf-cycle! growl-buf [:D4 :D4 0 :A4 :A4 0])

(buf-cycle! growl-amp-buf [1 1 0 1 1 0 1 1])
(buf-cycle! growl-buf [:A3 :A3 0 :E3 :E3 0 :G#3 :G#3])
(buf-cycle! growl-amp-buf [1 1 0 1 1])

(buf-cycle! growl-buf [:G3 :G3 0 :G3 :G3 0
                       :E3 :E3 0 :E3 :G3 0])

(buf-cycle! growl-buf [:E3 :E3 0 :E3 :E3 0
                       :A3 :A3 0 :A3 :A3 0])

(buf-cycle! growl-amp-buf [1 1 0 0 1 1 0 1 1])
(buf-cycle! growl-buf     [:C#3 :C#3 0 :A3 :A3 0 :C#3 :C#3])
(buf-cycle! growl-buf     [:B3 :B3 0 :D3 :D3 0 :F#3 :F#3])
(buf-cycle! growl-buf     [:D3 :D3 0 :D3 :D3])

(reset! color-l 0.1)
(reset! color-r 0.1)

(ctl growl-synth :amp 1.8)

(buf-cycle! shrill-seq-buf [1])

(buf-cycle! notes-buf [:A3 0 :A3])
(buf-cycle! notes-buf [:E3 0 :E3])

(buf-cycle! notes-buf    [:C#3 0 :C#3])
(buf-cycle! shrill-buf   [0 :A3 0 :A3 0])
(buf-cycle! shrill-pong-buf [0 0 :E3])

(buf-cycle! notes-buf    [:D3 0 :D3])
(buf-cycle! shrill-buf   [0 :F#3 0 :F#3 0])
(buf-cycle! shrill-pong-buf [0 0 :B3])

(ctl bass-g :amp 1.8)

(buf-cycle! shrill-seq-buf [0])
(buf-cycle! notes-buf [0])
(buf-cycle! shrill-buf [0])

(def fizzy-p (fizzy-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf notes-buf :duration-bus fizzy-duration))

(ctl growl-synth :amp 0)
(buf-cycle! growl-amp-buf [1])
(def g (growler [:head bass-g] :amp 0.8 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th) :note-buf growl-buf :growl-amp-buf growl-amp-buf))

(ctl time/root-s :rate 4)

(ctl bass-g :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

(buf-cycle! growl-buf  [:E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3
                        :E3 :E3 :E3     :E3 :E3 :E3

                        :C#3 :C#3 :C#3     :C#3 :C#3 :C#3
                        :C#3 :C#3 :C#3     :C#3 :C#3 :C#3

                        :F#3 :F#3 :F#3  :F#3 :F#3 :F#3
                        :F#3 :F#3 :F#3  :F#3 :F#3 :F#3

                        :A3 :A3 :A3  :A3 :A3 :A3
                        :A3 :A3 :A3  :A3 :A3 :A3

                        ;; :B3 :B3 :B3  :B3 :B3 :B3
                        ;; :B3 :B3 :B3  :B3 :B3 :B3
                        ])

(buf-cycle! fizzy-duration [1 1/2 1/2])
(buf-cycle! notes-buf  [0  0   :D4 0 0 :D4
                        0  0   :D4 0 0 :D4
                        0  0   :E4 0 0 :E4
                       :E4 :E4 :E4 0 0 :E4])

(buf-cycle! shrill-pong-final-buf
            [:G#4 :E4 :D4  :G#4 :E4 :D4
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

(buf-cycle! shrill-pong-final-buf
            [:A4 :E4 :G#4  :A4 :E4 :G#4
             :A4 :E4 :G#4  :A4 :E4 :G#4])

(buf-cycle! shrill-pong-final-buf
            [:G#3 :E3 :D3  :G#3 :E3 :D3
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

;;(kill high-hats)

(buf-cycle! white-seq-buf [0 0 0 1 1 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0
                           0 0 0 0 0 0])

;;(ctl white :amp 0.4)

(ctl time/root-s :rate 4)

(reset! space 0.9)
(ctl voice-g :note-buf shrill-pong-final-buf)
(ctl s :amp 1.2)

(buf-cycle! shrill-dur-buf [1/12])

(buf-cycle! shrill-dur-buf [1/8 1/16 1/16 1/8 1/16 1/16])
(buf-cycle! shrill-dur-buf [1/4 1/8 1/8 1/4 1/8 1/8])
(buf-cycle! shrill-dur-buf [1/2 1/4 1/4 1/2 1/4 1/4])

;;(mono-player moore-s :amp 1 :rate 1)
;;(echoey-buf :b moore-s)
;;(spacy moore-s)
;;(echoey-buf signals-s)
;;(spacy signals-s)

(buf-cycle! notes-buf [0])
(buf-cycle! growl-amp-buf [1 1 0 1 1 0 1 1])

(reset! color-l 0.0)
(reset! color-r 0.0)
(reset! space 0.0)
(reset! res 0.15)

(t/start-fullscreen "resources/shaders/zoomwave.glsl"
                    :textures [ :overtone-audio :previous-frame]
                    :user-data {"iLColor" color-l "iRColor" color-r
                                "iRes" res
                                "iSpace" space
                                "iA" (atom {:synth s :tap "a"})})

(comment
  (t/stop)
  (def fx2 (fx/fx-chorus 0))
  (def fx3 (fx/fx-echo 0))

  (kill fx2)
  (kill fx3)

  (kill whitenoise-hat)
  (kill kick2)
  (kill high-hats)
  (kill quick-kick)
  (kill mid-pings)
  (kill fizzy-pulsar)
  (kill pulsar)
  (kill glass-ping)
  (kill growl)
  (kill growler)

  (kill drums-g)
  (kill bass-g)

  (kill shrill-pong)
  (kill shrill-pulsar)
  (kill dark-ambience)

  (stop)
)
