(ns cassiopeia.destination.tsih
  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.mixers :as mix]
            [overtone.inst.synth :as s])
  (:use [overtone.live]
        [cassiopeia.samples]
        [cassiopeia.destination.tsih-orchestra]))

(stop)
(do
  (ctl time/root-s :rate 4)

  (defn buffer-cycle! [buf list]
    (buffer-write! buf (take (buffer-size buf) (cycle (map #(if (keyword? %) (note %) %) list)))))

  (defonce power-kick-g (group "Powerish kick"))
  (defonce power-kick-seq (buffer 16))

  (defonce melody-duration-b (buffer 128))
  (defonce melody-notes-b    (buffer 128))
  (defonce melody-pan-buf (buffer 128))

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

  (defonce white-seq-buf (buffer 32))
  (defonce white-notes-buf (buffer 32))
  (defonce white-g (group "whitenoise-hat"))

  (def shrill-seq-buf (buffer 32))
  (def shrill-dur-buf (buffer 32))

  (defonce fizzy-duration (buffer 128))

  (defonce shrill-pong-g (group "Shrill and flowey pong"))
  (defonce f-shrill-buf (buffer 128)))

(doall (map-indexed #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18)))

(doall (map-indexed #(glass-ping [:head glass-g] :amp 1 :note-buf bass-notes-buf :seq-buf ping-bass-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num (+ 2 %2)) (range 0 18)))

(defonce mid-ping-notes-buf (buffer 128))
(defonce mid-ping-seq-buf  (buffer 18))

(do
  (def mid-pings (doall (map-indexed #(glass-ping [:head mid-glass-g] :amp 1 :note-buf mid-ping-notes-buf :seq-buf mid-ping-seq-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

  (ctl p :amp 0)
  (ctl q :amp 0)
  (kill glass-g))

(kill high-pings)
(kill high-pings-echo)
(kill mid-pings)
(kill bazz)

(do
  (kill bazz-g)
  (kill power-kick-g)
  (kill kick2-g)
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

  (buffer-cycle! power-kick-seq [1 0 0 0 0 0 0 0])

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
  (buffer-cycle! kick-seq-buf [1 0 0 0])
  (buffer-cycle! white-seq-buf [0]))

(kill bazz)
(kill power-kick-g)
(kill kick2)
(ctl time/root-s :rate 0)

(mono-player moore-s :amp 1 :rate 0.95)

(defsynth echoey-buf [b 0 frames [256 :ir] out-bus 0 thresh 0.07]
  (let [in (play-buf 1 b (* (buf-rate-scale:kr b) 1.1))
        chain (fft (local-buf frames) in)
        chain (pv-mag-freeze chain -0.1)
        output (* (ifft chain) 0.9)
        output (+ output (comb-c:ar output 1 0.3 6))]
     (out out-bus output)))

(echoey-buf :b moore-s)

(kill white-g)
(doseq [i (range 0 32)]
  (whitenoise-hat
   [:head white-g]
   :note-buf white-notes-buf
   :amp (+ 0.1 (/  i 16))
   :seq-buf  white-seq-buf
   :beat-bus     (:count time/beat-1th)
   :beat-trg-bus (:beat time/beat-1th)
   :num-steps 32
   :beat-num i))

(buffer-cycle! white-seq-buf [1 0])
(buffer-cycle! white-seq-buf [1])
(buffer-cycle! white-seq-buf [0 0 0 1 1 0
                              0 0 0 0 0 0
                              0 0 0 0 0 0
                              0 0 0 0 0 0])

(kill whitenoise-hat)
(kill kick2-g)
(kill bazz)

(buffer-cycle! kick-seq-buf [0 0 1 0 0 1 0 0 1 1])

;;(ctl bazz-g :amp 0)

(buffer-cycle! ping-bass-seq-buf [1 0 1 0])
(buffer-cycle! bass-notes-buf (map note [:A1 :A1 :A1 :A1 :A1 :A1]))
(buffer-cycle! bass-notes-buf (map note [:A4 :A5 :A2 :A2 :A6 :A5]))
(buffer-cycle! bass-notes-buf (map note [:E4 :E5 :E2 :E2 :E5 :E4]))
(buffer-cycle! bass-notes-buf (map note [:D5 :D6 :D2 :D2 :D6 :D5]))

(buffer-write! bass-notes-buf (take 32 (cycle (map note [:A4 :E4 :E2 :E2 :D5 :D4
                                                         :E4 :E4 :A2 :A2 :D5 :D4
                                                         :D5 :D6 :A2 :E2 :D6 :D5]))))

(ctl growl-synth :amp 1)
(buffer-cycle! growl-amp-buf [1 1 0 1 1 0])
(buffer-cycle! growl-buf (map note [:D4 :D4 0 :A4 :A4 0]))

(buffer-cycle! mid-ping-seq-buf [1 1 0 0])
(buffer-cycle! mid-ping-seq-buf [0])

(ctl q :amp 0)
(ctl p :amp 0)

(buffer-cycle! mid-ping-notes-buf (map note [:A4 :A4 :D4 :D4 :D4 :E4]))
(buffer-cycle! mid-ping-notes-buf (map note [:A4 :A4 :D4 :D4 :D4 :E4
                                             0 0 0 0 0 0
                                             :A4 :A4 :D4 :D4 :D4 :F#4
                                             0 0 0 0 0 0
                                             :A4 :A4 :D4 :D4 :D4 :F#4
                                             0 0 0 0 0 0]))

(buffer-write! phase-bass-buf  (take 32 (cycle [0 1])))
(buffer-write! phase-bass-buf  (take 32 (cycle [1 1 0 0 0 0 0 0])))

(buffer-write! phase-bass-buf  (take 32 (cycle [1 1 0 0 0 0 0 0
                                                1 1 0 0 0 0 0 0
                                                1 1 0 0 0 0 0 0
                                                1 0 1 0 0 0 0])))

(buffer-cycle!  kick-seq-buf [1 0 0 0])

(defonce growl-amp-buf (buffer 128))
(defonce growl-buf (buffer 128))

(def p (pulsar :beat-trg-bus (:beat time/beat-1th)
               :beat-bus (:count time/beat-1th)
               :note-buf notes-buf
               :amp 0.7))

(def q (shrill-pulsar :beat-trg-bus (:beat time/beat-1th)
                      :beat-bus (:count time/beat-1th)
                      :note-buf shrill-buf
                      :amp 0.7))

(def growl-synth (growl :amp 1.5
                        :beat-trg-bus (:beat time/beat-4th)
                        :beat-bus (:count time/beat-4th)
                        :note-buf growl-buf
                        :growl-amp-buf growl-amp-buf))

(buffer-cycle! growl-amp-buf       [1   1   1   1   1   1   1   1   1   1])
(buffer-cycle! growl-buf (map note [:D3 :D3 :D3  :E3 :E3 :E3  :A4 :A4 :A4
                                    :D4 :D4 :D4  :F#4 :F#4 :F#4]))

(buffer-cycle! growl-amp-buf       [1   1   0  1   1])
(buffer-cycle! growl-buf (map note [:D4 :D4 0 :A4 :A4 0]))
(buffer-cycle! growl-buf (map note [:D3 :D3 0 :E3 :E3]))

(buffer-cycle! growl-buf (map note [:G3 :G3 0 :G3 :G3 0
                                    :E3 :E3 0 :E3 :G3 0]))

(buffer-cycle! growl-buf (map note [:E3 :E3 0 :E3 :E3 0
                                    :A3 :A3 0 :A3 :A3 0]))

(buffer-cycle! growl-buf (map note [:E3 :E3 0 :D3 :D3 0
                                    :F3 :F3 0 :A3 :A3 0]))

(buffer-cycle! growl-buf (map note [:D3 :D3 0 :D3 :D3]))

(ctl growl-synth :amp 1.8)

(s/rise-fall-pad :freq (midi->hz (note :A3)))

(def dark (dark-ambience :mul 0.4
                         :amp 0.4
                         :ring-freq (midi->hz (note :A3))))

(ctl dark :ring-freq (midi->hz (note :A3)))

;(kill dark-ambience)

(buffer-cycle! notes-buf (map note  [:F#3 0 :F#3]))
(buffer-cycle! notes-buf (map note  [:D3 0 :D3]))
(buffer-cycle! notes-buf (map note  [:E3 0 :E3]))

(buffer-cycle! notes-buf (map note  [:D3 0 :D3 :D3 0 :D3
                                     :D3 0 :D3 :F#3 0 :F#3
                                     :F#3 0 :F#3]))

;;(buffer-cycle! notes-buf (map note  [:D3 :D3 0 0]))

(buffer-cycle! shrill-buf (map note [0 :D3 0 :D3 0]))
(buffer-cycle! shrill-buf (map note [0 :C3 0 :C3 0]))

(buffer-cycle! notes-buf (cycle [0]))
(buffer-cycle! shrill-buf (cycle [0]))

;;(buffer-cycle! bass-notes-buf (map note [:A2]))

(kill shrill-pulsar)
(kill pulsar)
(kill growl)

;;PART 2

(def p (fizzy-pulsar :beat-trg-bus (:beat time/beat-1th)
                     :beat-bus (:count time/beat-1th)
                     :note-buf notes-buf
                     :duration-bus fizzy-duration))

(ctl growl-synth :amp 0)
(buffer-cycle! growl-amp-buf [1])
(def g (growler :amp 1.2
                :beat-trg-bus (:beat time/beat-4th)
                :beat-bus (:count time/beat-4th)
                :note-buf growl-buf
                :growl-amp-buf growl-amp-buf))

(ctl time/root-s :rate 4)

(ctl g :beat-bus (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th))

(ctl growl-synth :beat-bus (:count time/beat-1th)
                 :beat-trg-bus (:beat time/beat-1th))

(buffer-cycle! fizzy-duration [1/2 1/8 1/8])

(buffer-cycle! growl-buf  [:E3 :E3 :E3     :E3 :E3 :E3
                           :E3 :E3 :E3     :E3 :E3 :E3
                           :E3 :E3 :E3     :E3 :E3 :E3
                           :E3 :E3 :E3     :E3 :E3 :E3

                           :D3 :D3 :D3     :D3 :D3 :D3
                           :D3 :D3 :D3     :D3 :D3 :D3

                           :F#3 :F#3 :F#3  :F#3 :F#3 :F#3
                           :F#3 :F#3 :F#3  :F#3 :F#3 :F#3

                           :A3 :A3 :A3  :A3 :A3 :A3
                           :A3 :A3 :A3  :A3 :A3 :A3
                           ])

(buffer-cycle! notes-buf  [0  0   :D3 0 0 :D3
                           0  0   :D3 0 0 :D3
                           0  0   :E3 0 0 :E3
                          :E3 :E3 :E3 0 0 :E3])

(buffer-cycle! f-shrill-buf [:G#4 :E4 :D4  :G#4 :E4 :D4
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
                             :B5 :E5 :G#5  :A5 :E5 :G#5
                             ])

(buffer-cycle! growl-buf [:G#2 :G#2 :G#2  :G#2  :G#2  :G#2
                          :A3 :A3  :A3  :A3  :A3  :A3   ])

(buffer-cycle! f-shrill-buf [:G#3 :E3 :D3  :G#3 :E3 :D3
                             :G#3 :E3 :D3  :G#3 :E3 :A4
                             :G#3 :E3 :D3])


(ctl time/root-s :rate 4)
(buffer-cycle! kick-seq-buf [1 0 0 1 0 0
                             1 0 0 1 0 0
                             1 0 0 1 0 0
                             1 1 0 1 0 1])

(kill bazz-g)
(kill growler)

(ctl glass-g :amp 0)
(ctl mid-glass-g :amp 0)
(kill glass-g)
(kill mid-glass-g)

(doall (map-indexed
        #(shrill-pong
          [:head shrill-pong-g]
          :amp 1
          :note-buf shrill-buf
          :duration-bus shrill-dur-buf
          :seq-buf shrill-seq-buf
          :beat-bus (:count time/beat-1th)
          :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num %2) (range 0 32)))

(ctl shrill-pong-g :note-buf f-shrill-buf)

(buffer-cycle! shrill-seq-buf [1])
(buffer-cycle! shrill-dur-buf [1/12])

(buffer-cycle! shrill-dur-buf [1/8 1/16 1/16 1/8 1/16 1/16])
(buffer-cycle! shrill-dur-buf [1/4 1/8 1/8 1/4 1/8 1/8])
(buffer-cycle! shrill-dur-buf [1/2 1/4 1/4 1/2 1/4 1/4])

(kill shrill-pong-g)
(stop)

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
  (kill pulsar)
  (kill glass-ping)
  (kill bazz)
  (kill growl)
  (kill growler)
  (kill)

  (stop)
)
