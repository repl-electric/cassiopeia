(ns cassiopeia.destination.pop2
  (:use overtone.live)
  (:use mud.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  ;;  (:use dirt)
  (:require [mud.timing :as time]))

(ctl time/root-s :rate 7)

(defonce tonal-notes-b (buffer 256))
(defonce tonal-reverb-b (buffer 256))

(pattern! tonal-notes-b
          (repeat 8 (degrees [1 3 0 0] :minor :F3))
          (repeat 8 (degrees [3 4 0 0] :minor :F3))
          (repeat 8 (degrees [5 7 0 0] :minor :F3))
          (repeat 8 (degrees [7 8 0 0] :minor :F3)))

(do
  (defsynth tonal [amp 1 notes-buf 0
                   beat-trg-bus (:beat time/beat-1th)
                   beat-bus (:count time/beat-1th)]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)

          freq (midicps note)
          src (mix [(sin-osc freq)
                    (saw freq)
                    (blip freq (* 0.5 (sin-osc:kr 0.5)))])
          dly  (/ 1 freq)
          src (pluck src trg dly dly 0.9 0.4)
          src (rlpf src 1000)
          t 4
          mix-rate 0.8
          room-rate 0.5
          f-env      (env-gen (perc t t) trg 1 0 2)
          signal     (rlpf (* 0.3 src)
                           (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
          amt 0.3
          k          (/ (* 2 amt) (- 1 amt))
          distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))

          gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.5))))
          compressor (compander distort gate 0.01 1 0.5 0.01 0.01)

          dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
          reverb     (free-verb compressor mix-rate room-rate dampener)

          src (mix [(* 0.5 (rlpf (saw (* 1.0 freq)) 500)) src])

          src  (comb-n src 0.4 0.3 0.5)

          ]
      (out 0 (pan2 (* amp src)))))

  (kill tonal)
  (tonal :amp 0.6 :notes-buf tonal-notes-b :reverb-buf tonal-reverb-b))


(stop)

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf effects3-seq-buf]))

(pattern! effects-seq-buf [0])

(def bass-kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1
                                    :pattern effects-seq-buf :amp 0.1 :num-steps 8 :buf virus-kick-s) (range 0 16))))

(def kicks (doall (map #(seqer [:head drum-effects-g] :beat-num %1
                                                                        :pattern effects2-seq-buf :amp 0.5 :num-steps 8 :buf (buffer-mix-to-mono kick-s)) (range 0 16))))

(def bell (doall (map #(seqer [:head drum-effects-g] :beat-num %1
                                                                                                       :pattern effects3-seq-buf :amp 0.009 :num-steps 8 :buf (buffer-mix-to-mono bell-s)) (range 0 16))))

(pattern! effects-seq-buf  [0 1 0 0 0 0 1 0])
(pattern! effects2-seq-buf [1 0 0 0 1 0 0 0])
(pattern! effects3-seq-buf [1 0 0 0 1 0 0 0])


(kill drum-effects-g)

(stop)

(do
  (defsynth tonal [amp 1 notes-buf 0
                   beat-trg-bus (:beat time/beat-1th)
                   beat-bus (:count time/beat-1th)]
    (let [cnt (in:kr beat-bus)
          trg (in:kr beat-trg-bus)
          note (buf-rd:kr 1 notes-buf cnt)

          freq (midicps note)
          src (sum [(sin-osc freq)
                    (saw freq)
                    (blip freq (* 0.5 (sin-osc:kr 0.5)))])
          dly  (/ 1 freq)
          src (pluck src trg dly dly 0.9 0.4)
          src (rlpf src 1000)
          ;;          src (free-verb src 0.7 1 0)
          src (g-verb src 200 8)]
      (out 0 (pan2 (* amp src)))))

  (kill tonal)
  (tonal :amp 0.2 :notes-buf tonal-notes-b :reverb-buf tonal-reverb-b))
