(ns cassiopeia.sound-test
"Check the settings of audio equipment to sync correct volume levels."
(:use [overtone.live] [cassiopeia.waves.synths] [mud.core] [cassiopeia.waves.soprano] [cassiopeia.view-screen] [mud.core] [cassiopeia.engine.scheduled-sampler] [cassiopeia.samples] [cassiopeia.dirt] [cassiopeia.engine.samples])
(:require [mud.timing :as time] [overtone.studio.fx :as fx]  [shadertone.tone :as t] [cassiopeia.engine.buffers :as b]))

(ctl time/root-s :rate 4)

(do
  (defonce singers-g (group "singers"))
  (defonce note-buf (buffer 128))
  (defonce yeh-seq-buf (buffer 128))
  (doseq [i (range 0 3)] (slow-singer [:head singers-g] :note-buf note-buf :amp 2.9 :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th) :seq-b yeh-seq-buf :beat-num i :index-b yeh-index-buffer :num-steps 3))

  (pattern! yeh-seq-buf
            [0 0 0 0]
            [0 0 0 0]
            [0 0 0 0]
            [1 0 0 0]
            [1 0 0 0]
            [1 0 0 0])
  )

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 96 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf])
    (pattern! kick-seq-buf  [0])
    (pattern! bass-notes-buf (repeat 2 (repeat 4 [:B1 :B1 :B1 :B1]))
              (repeat 2 (repeat 4 [:E#1 :E#1 :E#1 :E#1]))
              (repeat 2 (repeat 4 [:F#1 :F#1 :F#1 :F#1]))))

(pattern! kick-seq-buf  [1 0 1 0 0 0 0 0])
(pattern! hats-buf      [0 0 0 0 0 0 1 1])
(pattern! white-seq-buf [0 1 1 0 1 0 1 1])
(pattern! effects-seq-buf (repeat 4 [1 0 0 0]))
(pattern! effects2-seq-buf [0 0 0 0] [1 1 1 1] [0 0 0 0] [1 0 1 1])
(pattern! hats-buf      (repeat 6 (concat (repeat 3 [0 1 0 0]) [1 1 0 0])))

(def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0 :amp 1)))
(ctl drums-g :mod-freq 10.2 :mod-index 0.1 :noise 0)

(def ghostly-snares (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects-seq-buf :amp 0.2 :num-steps 16 :buf (b/buffer-mix-to-mono snare-ghost-s)) (range 0 16))))

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1 :pattern effects2-seq-buf :amp 0.1 :num-steps 8 :buf (b/buffer-mix-to-mono deep-bass-kick-s)) (range 0 8))))

(def hats (doall (map #(high-hats [:head drums-g] :amp 0.2 :mix (nth (take 32 (cycle [1.0 1.0])) %1) :room 4 :note-buf bass-notes-buf :seq-buf hats-buf :num-steps 32 :beat-num %1) (range 0 32))))
(ctl hats :damp 1.9 :mix 0.2 :room 10 :amp 0.2)

(def white-hats (doall (map #(whitenoise-hat [:head drums-g] :amp 0.2 :seq-buf  white-seq-buf :num-steps 16 :beat-num %1) (range 0 16))))


(schedule-sample mm-low-s time/main-beat :mod-size 64 :amp 0.9)
(schedule-sample mm-high-s time/main-beat :mod-size 64 :amp 0.2)
(schedule-sample whisper-s time/main-beat :mod-size 64 :amp 0.09)
(mono-player moore-s :amp 1 :rate 1)
(mono-player the-sound-of-live-coding-s :amp 0.5 :rate 0.99)
(echoey-buf :b moore-s)
(spacy moore-s)
(echoey-buf signals-s)
(spacy signals-s :amp 2.3)
