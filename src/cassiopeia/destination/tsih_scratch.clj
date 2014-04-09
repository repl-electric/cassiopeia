(ns cassiopeia.destination.tsih-scratch
"
Experiments
"
(:require [cassiopeia.engine.timing :as time]
          [overtone.studio.fx :as fx]
          [cassiopeia.engine.mixers :as mix]
          [overtone.inst.synth :as s]
          [shadertone.tone :as t])
(:use [overtone.live]
;;      [cassiopeia.scratch]
      [cassiopeia.engine.core]
      [cassiopeia.samples]
      [cassiopeia.view-screen]
      [cassiopeia.waves.synths]))

(stop)


(do
  (ctl time/root-s :rate 4)

  (declare p q growl-synth)

  (defonce voice-g     (group "main voice"))
  (defonce bass-g      (group "bass voice"))
  (defonce drums-g     (group "drums"))
  (defonce glass-g     (group "glass"))
  (defonce mid-glass-g (group "A little more classey glass"))

  (defonce power-kick-seq-buf    (buffer 16))
  (defonce kick-seq-buf          (buffer 16))
  (defonce pulsar-buf            (buffer 128))
  (defonce shrill-buf            (buffer 128))
  (defonce growl-buf             (buffer 128))
  (defonce mid-ping-notes-buf    (buffer 32))
  (defonce mid-ping-seq-buf      (buffer 32))
  (defonce bass-notes-buf        (buffer 32))
  (defonce ping-bass-seq-buf     (buffer 32))
  (defonce phase-bass-buf        (buffer 32))
  (defonce white-seq-buf         (buffer 24))
  (defonce shrill-dur-buf        (buffer 32))
  (defonce fizzy-dur-buf         (buffer 128))
  (defonce shrill-pong-buf       (buffer 128))
  (defonce shrill-pong-final-buf (buffer 128)))


(do
  (kill high-hats)
  (kill quick-kick)
  (kill kick2)

  (reset! color-l 0.9)
  (reset! color-r 0.9)

  (def hats
    (doall (map #(high-hats
                  [:head drums-g]
                  :amp 0.09
                  :mix (nth (take 32 (cycle [1.0 1.0])) %1)
                  :room 4
                  :note-buf bass-notes-buf
                  :seq-buf phase-bass-buf
                  :beat-bus     (:count time/beat-1th)
                  :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num %1) (range 0 32))))
  (ctl hats :damp 1.9 :mix 0.9 :room 50)

  (kill hats)

  (def kick-seq-buf          (buffer 96))
  (def bass-notes-buf        (buffer 96))
  (def drums-g (group "drums"))

  (doseq [i (range 0 96)]
    (kick2
     [:head drums-g]
     :note-buf bass-notes-buf
     :seq-buf  kick-seq-buf
     :beat-bus      (:count time/beat-1th)
     :beat-trg-bus  (:beat time/beat-1th)
     :num-steps 96
     :beat-num i))

  (kill drums-g)

  (stop)

  (kill drums-g)

  (ctl drums-g :mod-freq 1)
  (ctl drums-g :mod-index 1.3 :noise 900)

  (pattern! bass-notes-buf (repeat 5 [:A1])
                           (repeat 2 [:A2]))
  (pattern! phase-bass-buf (repeat 4 (repeat 4 [1 0 0 0])) [1 1 1 1])
  (pattern! kick-seq-buf [1 0 0 0])
  (pattern! bass-notes-buf
            (repeat 3 (repeat 4 [:A1 :E2 :A1 :E2]))
            (repeat 4 [:A3 :E4 :A3 :E4])
            )

  (def white (doall (map
                     #(whitenoise-hat
                       [:head drums-g]
                       :amp 0.2
                       :seq-buf  white-seq-buf
                       :beat-bus     (:count time/beat-1th)
                       :beat-trg-bus (:beat time/beat-1th)
                       :num-steps 24
                       :beat-num %1) (range 0 24))))

  (kill whitenoise-hat)
  (pattern! white-seq-buf [1])
  (pattern! white-seq-buf (repeat 4 [1 0 0 0]) [0 0 1 1])

  (pattern! kick-seq-buf [0 0 1 0 0 1 0 0 1 1])

  (def s (shrill-pong [:head voice-g] :amp 1.1 :note-buf shrill-pong-buf :duration-bus shrill-dur-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th)))


  (pattern! shrill-dur-buf
            (repeat 4 (repeat 4 [1/12 1/12 1/2 1/2]))
            (repeat 4 (repeat 4 [1/12 1/12 1/12 1/4]))
            (repeat 4 [1/24 1/2 1/24 1/2]))

  (pattern! shrill-dur-buf
            (repeat 4 (repeat 4 [1/12 1/12 1/12 1/4]))
            (repeat 4 [1/4 1/4 1/4 1/4]))

  (def p (pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf pulsar-buf :amp 0.7))

  (def fizzy-p (fizzy-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf pulsar-buf :duration-bus shrill-buf))

;;  (def q (shrill-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf shrill-buf :amp 0.7))
;;  (kill q)

  (stop)

  ;; (:E3 :Ab3 :B3 :D4)
  ;; (:A3 :C#4 :E4 :Ab4)
  (let [[n1 n2 n3 n4] (chord-degree :v :A2 :major)
        [n11 n12 n13 n14] (chord-degree :i :A3 :major)]
    (pattern! pulsar-buf
              (repeat 4 (repeat 4 [0 0 0 0]))
              (repeat 4 [:F#4 :F#4 0 0])
              (repeat 4 [:G#4 :G#4 0 0]))
    (pattern! shrill-buf
              (repeat 4 (repeat 4 [n1 n1 n3 0]))   (repeat 4 [n3 n3 n4 0]))
    (pattern! shrill-pong-buf
              (repeat 3 [n1 n2 n2 n3])

              (repeat 1 [n11 n13 n14 n14])

              (repeat 3 [n1 n2 0 0])
              (repeat 1 [n11 n13 n14 69])
;;              (repeat 3 [n2 n2 n3 n3])
;;              (repeat 1 [n1 n2 n2 n3])
              ))


  (let [[n1 n2 n3 n4] (chord-degree :v :A2 :harmonic-minor)
        [n11 n12 n13 n14] (chord-degree :i :A3 :harmonic-minor)]
    (pattern! pulsar-buf
              (repeat 4 (repeat 4 [0 0 0 0]))
              (repeat 4 [:F#4 :F#4 0 0])
              (repeat 4 [:G#4 :G#4 0 0]))
    (pattern! shrill-buf
              (repeat 4 (repeat 4 [n1 n1 n3 0]))   (repeat 4 [n3 n3 n4 0]))
    (pattern! shrill-pong-buf
              (repeat 3 [n1 n2 n2 n3])

              (repeat 1 [n11 n13 n14 n14])

              (repeat 3 [n1 n2 0 0])
              (repeat 1 [n11 n13 n14 69])
              ;;              (repeat 3 [n2 n2 n3 n3])
              ;;              (repeat 1 [n1 n2 n2 n3])
              ))



(kill p q s)

(stop)

  (def growl-synth (growl [:head bass-g] :amp 1 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf growl-buf))

(note :A4)

  (pattern! growl-buf (degrees [1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1
                                3 3 3 3  3 3 3 3  3 3 3 3  3 3 3 3
                                5 5 5 5  5 5 5 5  5 5 5 5  5 5 5 5
                                6 6 6 6  6 6 6 6  6 6 6 6  6 6 6 6
                                7 7 7 7  7 7 7 7  7 7 7 7  7 7 7 7

                                8 8 8 8  8 8 8 8  8 8 8 8  8 8 8 8
                                ] :major :A3))

  (println  (map find-note-name (chord-degree :v :A2 :major)))
  (println (map find-note-name (chord-degree :i :A3 :major)))

  ;;:E4
  ;;F#4

  (kill growl-buf)

  (stop)








  (find-note-name 66) :F#4


  (ctl time/root-s :rate 4)
  (pattern! kick-seq-buf (repeat 3 [0 0 1 0 0 0])
                                   [0 1 1 1 0 0])
  (pattern! white-seq-buf [0]))

(stop)


(overtime! res 0.9 0.01)
(ctl time/root-s :rate 0)


(doall (map #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %1) (range 0 18)))

(do
  (def mid-pings (doall (map-indexed #(glass-ping [:head mid-glass-g] :amp 1 :note-buf mid-ping-notes-buf :seq-buf mid-ping-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

  (when (node-live? p) (ctl p :amp 0))
  (when (node-live? q) (ctl q :amp 0))

  (pattern! mid-ping-seq-buf [0])

  ;;(degrees->pitches [:iii] :major :A3)
  ;;(pattern! growl-buf (degrees [3 3 _ 8 8 _ 3 3] :major :A2))

  (pattern! growl-buf (degrees [1 1 1
                                3 3 3
                                5 5 5
                                6 6 6
                                8 8 8] :major :A3)))


(pattern! pulsar-buf [:A3 0 :A3])
(pattern! pulsar-buf [:E3 0 :E3])

(pattern! pulsar-buf       [:C#3 0 :C#3])
(pattern! shrill-buf      [0 0 :A3 0 :A3 0])
(pattern! shrill-pong-buf [0 0 :E3])

(pattern! pulsar-buf       [:D3 0 :D3])
(pattern! shrill-buf      [0 :F#3 0 :F#3 0])
(pattern! shrill-pong-buf [0 0 :B3])

(pattern! growl-buf  (map note [:A3  :A3  :A3
                                :E3  :E3  :E3
                                :C#3  :C#3 :C#3
                                :A4 :A4 :A4
                                :E3 :E3 :E3
                                :C#3  :C#3 :C#3]))

(kill glass-g)

(pattern! ping-bass-seq-buf [1 1 0 0])

(pattern! bass-notes-buf
          (repeat 3 (degrees [1 1 3 3 8 8 3 1] :major :A3))
          (repeat 1 (degrees [1 1 3 3 10 8 3 1] :major :A4)))

(pattern! bass-notes-buf [:A4 :A4 :A2 :A2 :A5 :A4 :A5])
(pattern! bass-notes-buf [:E4 :E4 :E2 :E2 :E5 :E4 :E5])
(pattern! bass-notes-buf [:C#2 :C#2 :A#3 :A#3 :C#3 :C#3])
(pattern! bass-notes-buf [:B2 :B2 :B5 :B6 :B2 :B6 :B5])

(pattern! bass-notes-buf [:A4 :E4 :E2 :E2 :D5 :D4
                          :E4 :E4 :A2 :A2 :D5 :D4
                          :C#5 :C#6 :A2 :E2 :C#6 :C#5])

(pattern! bass-notes-buf [:A1 :A1 :A1 :A1 :A1 :A1])

(pattern! growl-buf [:D4 :D4 0 :A4 :A4 0])

(ctl drums-g :amp 1)
(pattern! growl-buf
          (repeat 2 [:A4 :A4 :D4 :D4 :E4])
          (repeat 2 [:A4 :A4 :D4 :D4 :F#4]))

(pattern! kick-seq-buf     [1 1])
(pattern! mid-ping-seq-buf [1 1])
(pattern! mid-ping-notes-buf [:Ab4 :Ab4 :C#4 :C#4 :C#4 :E4])
(pattern! mid-ping-notes-buf  [:Ab4 :Ab4 :C#4 :C#4 :C#4 :E4
                              0 0 0 0 0 0
                              :Ab4 :Ab4 :C#4 :C#4 :C#4 :F#4
                              0 0 0 0 0 0
                              :Ab4 :Ab4 :C#4 :C#4 :C#4 :F#4
                               0 0 0 0 0 0])

(map find-note-name (chord-degree :v :A3 :major))

(pattern! phase-bass-buf [0 1])
(pattern! phase-bass-buf [1 1 0 0 0 0 0 0])

(pattern!  kick-seq-buf [1 0 0 0])

(overtime! color-r 0.9)
(def dark (dark-ambience :mul 0.2 :amp 0.4 :ring-freq (midi->hz (note :A3))))

(ctl dark :ring-freq (midi->hz (note :A3)))
(sing :note 64 :amp 1.09 :pos 1)
(sing :note 63 :amp 1.09 :pos -1)

(s/rise-fall-pad :freq (midi->hz (note :E3)))

;;(pattern! growl-buf (degrees [3 3 _ 8 8 _ 3 3] :major :A2))
(pattern! growl-buf [:C#3 :C#3 0 :Ab3 :Ab3 0 :B3 :B3 0 :C#3 :C#3])
;;(pattern! growl-buf [:B3 :B3 0 :D3 :D3 0 :F#3 :F#3])
(pattern! growl-buf [:D3 :D3 0 :D3 :D3])

(reset! color-l 0.0)
(reset! color-r 1.0)

(pattern! pulsar-buf [:A3 0 :A3])
(pattern! pulsar-buf [:E3 0 :E3])

(pattern! pulsar-buf      [:C#3 0 :C#3])
(pattern! shrill-buf      [0 0 :A3 0 :A3 0])
(pattern! shrill-pong-buf [0 0 :E3])

(let [[n1 n2 n3 n4] (chord-degree :iii :A2 :major)]
  (pattern! pulsar-buf      [0 n2 0])
  (pattern! shrill-buf      [0 n3 0 n2 0 n1 n4 0 0])
  (pattern! shrill-pong-buf [0 n1 0 n3 0 n2 0 n3 0]))

(pattern! pulsar-buf      [:D3 0 :D3])
(pattern! shrill-buf      [0 :F#3 0 :F#3 0])
(pattern! shrill-pong-buf [0 0 :B3])



(stop)


(pattern! pulsar-buf [0])
(pattern! shrill-buf [0])

(def fizzy-p (fizzy-pulsar :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :note-buf pulsar-buf :duration-bus fizzy-dur-buf))

(ctl growl-synth :amp 0)
(def g (growler [:head bass-g] :amp 0.8 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th) :note-buf growl-buf))

(ctl bass-g :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

(pattern! growl-buf  [:E3 :E3 :E3     :E3 :E3 :E3
                      :E3 :E3 :E3     :E3 :E3 :E3
                      :E3 :E3 :E3     :E3 :E3 :E3
                      :E3 :E3 :E3     :E3 :E3 :E3

                      :C#3 :C#3 :C#3  :C#3 :C#3 :C#3
                      :C#3 :C#3 :C#3  :C#3 :C#3 :C#3

                      :F#3 :F#3 :F#3  :F#3 :F#3 :F#3
                      :F#3 :F#3 :F#3  :F#3 :F#3 :F#3

                      :A3 :A3 :A3     :A3 :A3 :A3
                      :A3 :A3 :A3     :A3 :A3 :A3

                      ;; :B3 :B3 :B3  :B3 :B3 :B3
                      ;; :B3 :B3 :B3  :B3 :B3 :B3
                      ])

(pattern! fizzy-dur-buf [1 1/2 1/2])
(pattern! pulsar-buf  [0  0    :D4 0 0 :D4
                       0  0    :D4 0 0 :D4
                       0  0    :E4 0 0 :E4
                       :E4 :E4 :E4 0 0 :E4])

(pattern! shrill-pong-final-buf
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

(pattern! shrill-pong-final-buf
            [:A4 :E4 :G#4  :A4 :E4 :G#4
             :A4 :E4 :G#4  :A4 :E4 :G#4])

(pattern! shrill-pong-final-buf
            [:G#3 :E3 :D3  :G#3 :E3 :D3
             :G#3 :E3 :D3  :G#3 :E3 :A4
             :G#3 :E3 :D3])

(ctl glass-g :amp 0)
(ctl mid-glass-g :amp 0)
(kill glass-g)
(kill mid-glass-g)

;;(kill high-hats)

(pattern! kick-seq-buf (repeat 3 [1 0 0 1 0 0]) [1 1 0 1 0 1])
(pattern! white-seq-buf [0 0 0 1 1 0] (repeat 3 [0 0 0 0 0 0]))

;;(ctl drums-g :amp 0.1)
;;(ctl white :amp 0.4)

(ctl time/root-s :rate 4)

(overtime! space 0.0 0.05)
(ctl voice-g :note-buf shrill-pong-final-buf)
;;(ctl s :amp 1.2)

(pattern! shrill-dur-buf [1/12])

(pattern! shrill-dur-buf [1/8 1/16 1/16])
(pattern! shrill-dur-buf [1/4 1/8 1/8])
(pattern! shrill-dur-buf [1/2 1/4 1/4])

;;(mono-player moore-s :amp 1 :rate 1)
;;(echoey-buf :b moore-s)
;;(spacy moore-s)
;;(echoey-buf signals-s)
;;(spacy signals-s :amp 2.2)

(pattern! pulsar-buf [0])
(pattern! growl-buf  [:D3 :D3 0 :D3 :D3])

(reset! color-l 0.0)
(reset! color-r 0.0)
(reset! space 0.0)
(reset! res 0.15)

;;(def view-port "journey.glsl")
(def view-port "zoomwave.glsl")

(t/start-fullscreen (str "resources/shaders/" view-port)
                    :textures [:overtone-audio :previous-frame]
                    :user-data {"iLColor" color-l "iRColor" color-r
                                "iRes" res
                                "iSpace" space
                                "iA" (atom {:synth s :tap "a"})
                                "iG" (atom {:synth growl-synth :tap "g"})})




























(stop)









(on-trigger (:trig-id time/main-beat)
            (fn [x]
              (when (= 0 (mod x 8))




                )


              )
            ::test-trig

            )

(remove-event-handler ::test-trig)
