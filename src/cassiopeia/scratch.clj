(ns cassiopeia.scratch
  "Here lies demons and a splatter gun of ideas and experiments."
  (:use overtone.live)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.soprano)
  (require [cassiopeia.engine.timing :as time]))

(defsynth whoosher [freq 400 out-bus 0 swish 970 amp 0.1]
  (let [whoosh (lag
                (env-gen:kr (envelope [0.8 1.0 0.9 0.7 0.5 0.3 0.24 0.12 0.0]
                                      [0.08 0.2 0.15 0.11 0.08 0.09 0.06 0.18]
                                      2)
                            :time-scale (* 1.3 1.5))
                0.2)
        whoosh-vol (env-gen:kr (envelope [0.0, 0.2, 0.7, 0.9, 1.0, 0.8, 0.6, 0.43, 0.22, 0.0]
                                         [0.4, 0.5, 0.2, 0.15, 0.17, 0.11, 0.1, 0.16, 0.28]
                                         2)
                               :time-scale (* 0.9 1.5))

        whoosh-vol (* (* 1.5 (lag whoosh-vol 0.2)) (lag (+ 1 (* 0.2 (lf-noise1:kr 37.2))) 0.2))
        noise (mix (comb-c:ar (* 0.2 (white-noise:ar)) 0.1 [(lin-exp whoosh 0.16 0.00011)
                                                            (lin-exp whoosh 0.19 0.00013)] 0.09))
        noise (+ noise (brown-noise))

        trump (* freq (range-lin whoosh 0.92 1.08))
        trump (* 0.6 (mix [(* 0.8 (sin-osc-fb:ar (* trump (lag (+ 1 (* 0.06 (lf-noise0 28))) 0.8) (+ 0.8 (* 0 0.05))) 1.2))
                           (* 0.8 (sin-osc-fb:ar (* trump (lag (+ 1 (* 0.06 (lf-noise0 28))) 0.8) (+ 0.8 (* 1 0.05))) 1.2))
                           (* 0.8 (sin-osc-fb:ar (* trump (lag (+ 1 (* 0.06 (lf-noise0 28))) 0.8) (+ 0.8 (* 2 0.05))) 1.2))]))
        chain (fft (local-buf 2048) trump)
        chain (pv-bin-shift chain (range-lin (white-noise:kr) 0.96 1.01) (range-lin whoosh 24 28))
        trump (* (ifft chain) 0.2)

        src (+ noise trump)
        src (* src whoosh-vol)
        panning (line:kr -0.9 0.9 3)]
    (out out-bus (* amp (pan2 src panning 1.0)))))

(kill whoosher)

(defonce note-buf (buffer 128))
(defonce note2-buf (buffer 128))

(defonce seq-b1 (buffer 128))
(defonce seq-b2 (buffer 128))
(defonce seq-b3 (buffer 128))
(defonce seq-b4 (buffer 128))

(def singers-g (group "singers"))

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

(kill singers-g)
;;(map find-note-name [63 62 64 62 62 62 64])
(pattern! note-buf     '(:Eb4 :D4 :E4 :D4 :D4 :D4 :E4))

;;(pattern! b    '(68 66 65 63 64))
(pattern! note-buf  (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4]))

(on-trigger (:trig-id time/main-beat)
            (fn [x]
              (when (= 0 (mod x 16)))
              (pattern! b     (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])))
            ::test-trig2)

(require '[overtone.inst.sampled-piano :as sp])

(on-trigger (:trig-id time/main-beat)
            (fn [x]
              (when (= 0.0 (mod x 16))
                (pattern! b     (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4]))))
            ::test-trig4)


(remove-event-handler ::test-trig4)

(ctl singers-g :attack 0.2)
(ctl singers-g :release 6.)

(def singers (doseq [i (range 0 3)]
               (slow-singer
                [:head singers-g]
                :note-buf note2-buf :amp 2.9
                :beat-b (:beat time/beat-12th) :count-b (:count time/beat-12th)
                :seq-b seq-b2
                :beat-num i
                :num-steps 3)))

(pattern! note2-buf [67 66 68])
(pattern! seq-b2 [1 1 1])

(def fast-singing (fast-singer
                   [:head singers-g]
                   :note-buf note-buf :amp 0.4
                   :beat-b (:beat time/beat-1th) :count-b (:count time/beat-1th)))

(node-over-time fast-singing :amp 0.0 0.39 0.01)

(kill fast-singing)

(ctl fast-singing :amp 0.2)

(pattern! note-buf
          [63 64 62 70 70]
          [63 64 62 70 70]
          [62 63 64 70 70]
          [62 63 64 70 70]

          [70 68 70 70 70]
          [70 68 70 70 70]
          )

(singer :note-buf b :amp 0.4 :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th) :release 10)

(kill singer)
(kill slow-singer)


(comment
  (sing :note 60 :amp 1.49 :pos 0)
  (sing :note 64 :amp 1.09 :pos 0)
  (sing :note 67 :amp 1.09 :pos 0)
  (sing :note 68 :amp 1.09 :pos 0))

(comment
  (def d (dark-ambience))

  ;;(kill dark-ambience)

  (def space-p (space-ping :freq-limit-buf freq-limit-buf
                           :beat-bus (:beat time/main-beat)
                           :amp 2))

  (ctl space-p :amp 0)
  (pattern! freq-limit-buf (repeat 3 [4.9 4.9 0.4 0.4]))

  (deep-space-signals)

  (ctl d :ring-freq (midi->hz (note :A3)))

  (kill deep-space-signals)
  (kill space-ping)

  (sing :note 60 :amp 0.2 :pos 1)
  (sing :note 61 :amp 0.2 :pos 1)

  (sing :note 62 :amp 0.2 :pos -1)
  (sing :note 64 :amp 0.2)
  (sing :note 66 :amp 0.2)
  (sing :note 69 :amp 0.2 :pos 1)


  (let [[n1 n2 n3 n4] (chord-degree (rand-nth (keys (dissoc DEGREE :_))) (rand-nth [:A2 :A1]) :major)]
    (pattern! pulsar-buf      [0 n2 0])
    (pattern! shrill-buf      [0 n3 0 n2 0 n1 n4 0 0])
    (pattern! shrill-pong-buf [0 n1 0 n3 0 n2 0 n3 0]))

  (whoosher))
(stop)
