(ns cassiopeia.destination.flatiron
 " .-. .   .-. .-. .-. .-. .-. .  .
   |-  |   |-|  |   |  |(  | | |\\|
   '   `-' ` '  '  `-' ' ' `-' ' ``"(:use [overtone.live][mud.core][mud.chords][cassiopeia.waves.synths][cassiopeia.samples][cassiopeia.engine.buffers][cassiopeia.dirt][cassiopeia.waves.buf-effects][cassiopeia.engine.expediency][cassiopeia.destination.flatiron.scores][cassiopeia.engine.scheduled-sampler])(:require [mud.timing :as time][clojure.math.numeric-tower :as math][overtone.studio.fx :as fx] [cassiopeia.destination.flatiron.utils :as fl]))
(overtime! splatter 500.0 1500.0)
(ctl-global-clock 0.2)

(nyc)
(fl/v 3.0)

(one-time-beat-trigger
 15 16
 (fn []
   (do
     (pattern! hats-buf     (repeat 3 [0 0 0 0  1 0 0 0  0 0 1 0  0 0 0 0])
                                      [0 0 0 0  1 0 0 0  0 0 1 0  1 0 0 0])
     (pattern! kick-seq-buf (repeat 3 [1 0 0 0  0 0 0 0  1 0 0 0  0 0 0 0])
                                      [1 0 0 0  0 0 0 0  1 0 0 0  1 0 1 0])

     (def white (whitenoise-hat [:head drums-g] :amp-buf hats-amp :seq-buf hats-buf :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :release 0.1 :attack 0.0 :beat-num 0))
     (ctl white :amp-buf hats-amp)
     (ctl white :attack 0.04 :release 0.01 :amp 1)
     (ctl white :attack 0.002 :release 0.04 :amp 2)
     (ctl kicker :amp 1.0)
     )))

(on-beat-trigger 32 #(do (stereo-player click-s :amp 1)))
(n-overtime! nomad-chord-g :saw-cutoff 2000 10 50)
(pattern! nomad-start (def p
                        (concat
                         (degrees-seq [:f4 1311 :c3 66 :f3 11])
                         (degrees-seq [:f4 1311 :c3 66 :f3 77])
                         (degrees-seq [:f3 1311 :c3 66 :f3 77])
                         )))

(pattern! nomad-start (map (fn [z] (cond
                                    (= (list z) (degrees [7] :minor :f3)) (degrees [1] :minor :f3)
                                    ;;                                   (= (list z) (degrees [4] :minor :f4)) (degrees [1] :minor :f3)
                                    ;;                                   (= (list z) (degrees [4] :minor :f4)) (degrees [1] :minor :f3)
                                    :else z
                                    )) (flatten p)))

(one-time-beat-trigger
 126 128
 (fn [] ;;DARKER PROGRESSION
   (do
     (plain-space-organ :tone (/ (midi->hz (note :F0)) 2) :duration 8 :amp 0.45)
     (doseq [loud-thing [nomad-chord-g drum-effects drums-g]](ctl loud-thing :amp 0.00))

     (def noho-chord-g (chord-synth general-purpose-assembly 3 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :wave 1 :attack 1.0 :release 5.0))

     (chord-pattern noho-chord-g apeg-swell)
     (chord-pattern flatiron-chord-g dark-chords-score)
     (chord-pattern nomad-chord-g darker-pinger-score)
     )
   (doseq [s (:synths nomad-chord-g)]
     (ctl s :amp 0.00 :saw-cutoff 100 :wave 0 :attack 1.0 :release 5.0)
     (n-overtime! s :saw-cutoff 100 2000 50)
     (n-overtime! s :amp 0.00 0.24 0.03))
   (when beat-tap (remove-watch beat-tap :cell-color))))

(ctl noho-chord-g :amp 0.6 :saw-cutoff)
(ctl nomad-chord-g :amp 0.228 :saw-cutoff  :wave 1)
(pattern! kick-seq-buf [1 0 0 0 1 0 0 0])
(ctl kicker :amp 0)
(ctl white :amp 0)

(do
  (overtime! circle-slice (* 0.5 Math/PI))(reset! cells-weight 4.0)
  (def westvil-chord-g (chord-synth general-purpose-assembly 4 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1))
  (chord-pattern! westvil-chord-g pinger-score-spair)

  (ctl noho-chord-g :amp 0.0)
  (ctl westvil-chord-g :amp 0.00 :saw-cutoff 2000 :wave 2 :attack 1.0 :release 5.0)
  (n-overtime! westvil-chord-g :amp 0 0.24 0.06)

  (chord-pattern westvil-chord-g pinger-growth-score-spair)
  (ctl drum-effects-g :amp 0.3) (ctl drums-g :amp 1.0)

  (pattern! effects-seq-buf  (repeat 12 [1 0])  [1 0 0 0])
  (ctl nomad-chord-g :amp 0.3 :saw-cutoff 2600 :wave 0 :attack 1.0 :release 5.0)
  (def f (dulcet-fizzle :amp 2.0 :note-buf df-b))
)

(do
  (ctl westvil-chord-g :amp 0)
  (ctl-time westvil-chord-g time/beat-2th)
  (ctl-time nomad-chord-g time/beat-2th)
  (ctl-time flatiron-chord-g time/beat-1th)

  (chord-pattern flatiron-chord-g pinger-score)

  (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.1])
        _ (pattern! sd-release-b [1.0 0.6 0.4 0.2])
        _ (pattern! sd-amp-b     [1.2 0.9 0.9 0.8])]
    (chord-pattern nomad-chord-g chords-score)))

(do
  (pattern! kick-seq-buf
            (repeat 3 (concat [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 0 0]))
            [1 0 0 0 1 0 0 0] [1 0 0 0 1 0 1 0])
  (def f (dulcet-fizzle :amp 2.0 :note-buf df-b)))

(one-time-beat-trigger
 126 128
 (fn [& _]
   (ctl-time nomad-chord-g time/beat-1th)
   (ctl-time westvil-chord-g time/beat-1th)
   (ctl-time flatiron-chord-g time/beat-2th)

   (one-time-beat-trigger
    127 128
    (fn [& _]
      (def nolita-chord-g
        (chord-synth general-purpose-assembly 4 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :attack 0.1 :release 0.1))

      (chord-pattern nolita-chord-g pinger-score-alternative)

      (ctl-time nomad-chord-g time/beat-1th)
      (ctl-time westvil-chord-g time/beat-1th)
      (ctl-time flatiron-chord-g time/beat-2th)

      (ctl noho-chord-g :amp 0.18)
      (ctl nolita-chord-g :amp 0.18)
      (chord-pattern noho-chord-g pinger-score-spair)
      (n-overtime! nolita-chord-g :saw-cutoff 0.0 1000 50)
      (n-overtime! westvil-chord-g :saw-cutoff 0.0 2600 50)
      (n-overtime! noho-chord-g    :saw-cutoff 0.0 1000 50)

      (chord-pattern westvil-chord-g pinger-growth-score-spair)
      (chord-pattern nomad-chord-g   pinger-score-highlighted)

      (let [_ (pattern! sd-attack-b  [0.06 0.12 0.12 0.12])
            _ (pattern! sd-release-b [1.0  1.0 1.0 1.0])
            _ (pattern! sd-amp-b     [1.2  1.0 1.0 1.0])]
        (chord-pattern flatiron-chord-g chords-score))
))))

;;More fizzle
;;(doall (map #(n-overtime! % :saw-cutoff 2600.0 0 50) (:synths nomad-chord-g)))

(do
  (def brooklyn-chord-g (chord-synth general-purpose-assembly 3 :amp 0.0 :noise-level 0.05 :beat-trg-bus (:beat time/beat-2th) :beat-bus (:count time/beat-2th) :attack 0.1 :release 0.1))
  (reset! color 0.5)
  (chord-pattern brooklyn-chord-g  darker-pinger-score)
  (ctl brooklyn-chord-g :amp 0.18 :saw-cutoff 1000)
  (ctl noho-chord-g :saw-cutoff 300 :amp 0.18)
  (chord-pattern noho-chord-g apeg-swell))

(defn nyc
  "New york city"
  []
  (do (def master-vol 3.0) (volume master-vol) (fl/v master-vol))
  (ctl-global-clock 0.0)
  (defbufs 256 [df-b s-note-b])

  (do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf effects-seq-buf]) (defonce hats-amp (buffer 256)) (defonce kick-amp (buffer 256)) (defonce kick-seq-buf (buffer 256)))
  (pattern! kick-amp  [1.5 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1] (repeat 2 [1.2 1 1 1 1 1 1 1   1.1 1 1 1 1 1 1 1]) [1.2 1 1 1 1 1 1 1   1.2 1 1 1 1.2 1 1.3 1])
  (pattern! hats-amp  (repeat 3 [2 2 2 2 2.1 2 2 2   2 2 2 2 2 2 2 2]) [2 2 2 2 2.1 2 2 2   2 2 2.4 2 2.4 2 2 2])
  (pattern! bass-notes-buf
            (repeat 8 (degrees [1] :minor :F1))
            (repeat 2 (repeat 8 (degrees [1] :minor :F1)))
            (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
            (repeat 2 (repeat 8 (degrees [3] :minor :F1)))
            [(degrees [1 1 1 1  5 4 3 1] :minor :F1)])

  (def beats (buffer->tap-lite kick-seq-buf (:count time/beat-1th) :measure 8))
  (defonce circle-count        (atom 4.0))
  (defonce color               (atom 0.1))
  (defonce circle-slice        (atom 8.0))
  (defonce circle-growth-speed (atom 0.1))
  (defonce circle-edge         (atom 0.1))
  (defonce circular-weight     (atom 0.0))
  (defonce population-weight   (atom 0.0))
  (defonce cells-weight        (atom 0.0))
  (defonce nyc-weight          (atom 0.0))
  (defonce invert-color        (atom 1.0))
  (defonce cell-dance-weight   (atom 1.0))
  (defonce splatter            (atom 500000.0))
  (defonce circle-intensity    (atom 0.0025))

  (defonce buffer-change-event-nomad (atom 0.0))
  (def ibeat (atom {:synth beats :tap "beat"}))
  (def beat-tap (get-in (:synth @ibeat) [:taps (:tap @ibeat)]))
  (def cell-dance-color (atom 0.01))
  (add-watch beat-tap :cell-color
             (fn [_ _ old new]
               (when (and (= old 0.0) (= 1.0 new))
                 (reset! cell-dance-color (mod (+ @cell-dance-color 1.0) 100)))))

  (def nomad-chord-g nil)
  (add-watch
   buffer-change-event-nomad
   :buffer-change-event-nomad
   (fn [& _]
     (when nomad-chord-g
       (let [n (int (buffer-get (second (:bufs nomad-chord-g)) 0))]
         (case n
           29 (do (overtime! circle-edge 0.0)  (overtime! nyc-weight 0.0 0.005))
           32 (do (overtime! circle-edge -0.3) (overtime! nyc-weight 0.004))
           34 (do (overtime! circle-edge -0.5) (overtime! nyc-weight 0.01))
           36 (do (overtime! circle-edge -0.6) (overtime! nyc-weight 0.02))

           41 (do (overtime! circle-edge 0.0)  (overtime! nyc-weight 0.0 0.005))
           44 (do (overtime! circle-edge -0.3) (overtime! nyc-weight 0.004))
           46 (do (overtime! circle-edge -0.5) (overtime! nyc-weight 0.01))
           48 (do (overtime! circle-edge -0.7) (overtime! nyc-weight 0.02))

           (do (reset! circle-edge 0.0) (reset! nyc-weight 0.0)))))))

  ;;(remove-watch buffer-change-event-nomad :buffer-change-event-nomad)

  (reset! cell-dance-weight 1.0)
  (reset! splatter 500000.0)

  (fl/v 3.0)
  (ctl-global-clock 0.2)

  ;;(kill beats)
  (start-graphics "resources/shaders/nyc.glsl"
                  :textures [:overtone-audio :previous-frame
                             "resources/textures/tex16.png"]
                  :user-data {"iGlobalBeatCount" (atom {:synth beats :tap "global-beat-count"})
                              "iBeat"            ibeat

                              "iColor" color
                              "iCircleCount" circle-count
                              "iHalfPi" circle-slice
                              "iInOutSpeed" circle-growth-speed
                              "iDeformCircles" circle-edge
                              "iCircularWeight"  circular-weight
                              "iPopulationWeight" population-weight
                              "iBouncingWeight"   cells-weight
                              "iNycWeight" nyc-weight
                              "iInvertColor" invert-color
                              "iCircleDanceWeight" cell-dance-weight
                              "iCircleDanceColor" cell-dance-color
                              "iDeath" fl/vol
                              "iSplatter" splatter
                              "iCircleDistort" circle-intensity
                              }))
(comment
  (stop-graphics "resources/shaders/nyc.glsl")
  (stop-everything!)
  (stop)
  )
