(ns cassiopeia.scratch
  "Here lies demons and a splatter gun of ideas and experiments."
  (:use overtone.live)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.soprano)
  (require [cassiopeia.engine.timing :as time]
           [overtone.studio.fx :as fx]))

(do
  (def singers-g (group "singers"))

  (defonce note-buf (buffer 128))
  (defonce note2-buf (buffer 128))

  (defonce seq-b1 (buffer 128))
  (defonce seq-b2 (buffer 128))
  (defonce seq-b3 (buffer 128))
  (defonce seq-b4 (buffer 128)))

(def singers-ah-p (doseq [i (range 0 3)]
                    (slow-singer
                     [:head singers-g]
                     :note-buf note-buf :amp 2.9
                     :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                     :seq-b seq-b1
                     :beat-num i
                     :index-b ah-index-buffer
                     :num-steps 3)))

(def singers-ah-f (doseq [i (range 0 3)]
                    (slow-singer
                     [:head singers-g]
                     :note-buf note-buf :amp 2.9
                     :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                     :seq-b seq-b3
                     :beat-num i
                     :index-b ah-strong-index-buffer
                     :num-steps 3)))

(def singers-yeh (doseq [i (range 0 3)]
                   (slow-singer
                    [:head singers-g]
                    :note-buf note-buf :amp 2.9
                    :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                    :seq-b seq-b4
                    :beat-num i
                    :index-b yeh-index-buffer
                    :num-steps 3)))

(pattern! seq-b1
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [1 0 0 0]
          [1 0 0 0])

(pattern! seq-b3
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [1 0 0 0]
          [1 0 0 0])

(pattern! seq-b4
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [1 0 0 0]
          [0 0 0 0])

;;63 62 64 62 62 62 64
(pattern! note-buf [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])
(pattern! note-buf [70 70 69 69  68 68 70 70])
(pattern! note-buf (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4]))

(on-trigger (:trig-id time/main-beat)
            (fn [x]
              (when (= 0 (mod x 16)))
              (pattern! b     (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])))
            ::shuffle-trig)

(remove-event-handler ::shuffle-trig)

(ctl singers-g :attack 0.2)
(ctl singers-g :release 6.)

(pattern! note2-buf [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])
(pattern! seq-b2 [1 1 1])

(def fast-singing (fast-singer
                   [:head singers-g]
                   :note-buf note2-buf :amp 0
                   :beat-b (:beat time/beat-1th) :count-b (:count time/beat-1th)))

(pattern! note-buf [70 70 69 69  68 68 70 70])

(node-overtime fast-singing :amp 0.0 0.35 0.01)
(ctl fast-singing :release 0.1 :attack 0.1)
(ctl fast-singing :release 0.4 :attack 1.0)

(kill fast-singing)

(comment
  (sing :note 60 :amp 1.49 :pos 0)
  (sing :note 64 :amp 1.09 :pos 0)
  (sing :note 67 :amp 1.09 :pos 0)
  (sing :note 68 :amp 1.09 :pos 0)

  (fx/fx-chorus)
  (fx/fx-reverb)
  (fx/fx-limiter)
  (fx/fx-compressor)
  (fx/fx-rlpf)
  (fx/fx-noise-gate)

  (fx/fx-distortion-tubescreamer)
  (fx/fx-echo))
(stop)
