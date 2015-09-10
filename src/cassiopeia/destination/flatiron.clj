(ns cassiopeia.destination.flatiron
 " .-. .   .-. .-. .-. .-. .-. .  .
   |-  |   |-|  |   |  |(  | | |\\|
   '   `-' ` '  '  `-' ' ' `-' ' ``"(:use [overtone.live][mud.core][mud.chords][cassiopeia.waves.synths][cassiopeia.samples][cassiopeia.engine.buffers][cassiopeia.dirt][cassiopeia.waves.buf-effects][cassiopeia.engine.expediency][cassiopeia.destination.flatiron.scores][cassiopeia.engine.scheduled-sampler])(:require [mud.timing :as time][clojure.math.numeric-tower :as math][overtone.studio.fx :as fx] [cassiopeia.destination.flatiron.utils :as fl]))
(ctl-global-clock 0.2)

(nyc)


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
  (defonce stars-weight        (atom 0.0))
  (defonce circle-count        (atom 4.0))
  (defonce color               (atom 0.1))
  (defonce circle-slice        (atom 8.0))
  (defonce circle-edge         (atom 0.1))
  (defonce circular-weight     (atom 0.0))
  (defonce population-weight   (atom 0.0))
  (defonce cells-weight        (atom 0.0))
  (defonce nyc-weight          (atom 0.0))
  (defonce invert-color        (atom 1.0))
  (defonce cell-dance-weight   (atom 1.0))
  (defonce splatter            (atom 1.0))
  (defonce circle-intensity    (atom 0.0025))
  (defonce buffer-change-event-nomad (atom 0.0))
  (defonce break-parens-in-case-of-emergency (atom 0.0))
  (def ibeat (atom {:synth beats :tap "beat"}))
  (def beat-tap (get-in (:synth @ibeat) [:taps (:tap @ibeat)]))
  (def cell-dance-color (atom 0.01))
  (add-watch beat-tap :cell-color
             (fn [_ _ old new]
               (when (and (= old 0.0) (= 1.0 new))
                 (reset! cell-dance-color (mod (+ @cell-dance-color 1.0) 100)))))

  (defonce converge-zoom-rate (atom 0.2))
  (reset! converge-zoom-rate 0.2)
  (reset! splatter 1.0)
  (add-watch beat-tap :converge-circles
             (fn [_ _ old new]
               (when (>= @cell-dance-weight 8.0)
                 (reset! splatter (max 0 (- @splatter @converge-zoom-rate))))
               (reset! converge-zoom-rate (max 0.0005 (* @converge-zoom-rate 0.5)))
               (when (<= @splatter 0.0)
                 (remove-watch beat-tap :converge-circles)
                 )))

  (reset! circle-count 4.0)
  (reset! color               0.1)
  (reset! circle-slice        8.0)
  (reset! circle-edge         0.1)
  (reset! circular-weight     0.0)
  (reset! population-weight   0.0)
  (reset! cells-weight        0.0)
  (reset! nyc-weight          0.0)
  (reset! invert-color        1.0)
  (reset! cell-dance-weight   1.0)
  (reset! splatter            1.0)
  (reset! circle-intensity    0.0025)
  (reset! buffer-change-event-nomad 0.0)
  (reset! cell-dance-color 0.01)
  (reset! break-parens-in-case-of-emergency 0.0)

  (def nomad-chord-g nil)
  (add-watch
   buffer-change-event-nomad
   :buffer-change-event-nomad
   (fn [& _]
     (when nomad-chord-g
       (let [n (int (buffer-get (second (:bufs nomad-chord-g)) 0))]
         (case n
           29 (do (overtime! circle-edge 0.0)  (overtime! nyc-weight 0.0 0.005) (reset! invert-color 1.0))
           32 (do (overtime! circle-edge -0.3) (overtime! nyc-weight 0.004) (reset! invert-color 2.0))
           34 (do (overtime! circle-edge -0.5) (overtime! nyc-weight 0.01))
           36 (do (overtime! circle-edge -0.55) (overtime! nyc-weight 0.0119))

           41 (do (overtime! circle-edge 0.0)  (overtime! nyc-weight 0.0 0.005) (reset! invert-color 1.0))
           44 (do (overtime! circle-edge -0.3) (overtime! nyc-weight 0.004) (reset! invert-color 2.0))
           46 (do (overtime! circle-edge -0.5) (overtime! nyc-weight 0.01))
           48 (do (overtime! circle-edge -0.5) (overtime! nyc-weight 0.012))

           53 (do (overtime! circle-edge -0.5) (reset! nyc-weight 0.015))

           (do (reset! circle-edge 0.0) (reset! nyc-weight 0.0)))))))

  ;;(remove-watch buffer-change-event-nomad :buffer-change-event-nomad)

  (reset! cell-dance-weight 1.0)
  (reset! splatter 1.0)

  (fl/v 3.0)
  (ctl-global-clock 0.2)

  ;;(kill beats)
  (start-graphics "resources/shaders/nyc.glsl"
                  :textures [:overtone-audio :previous-frame
                             "resources/textures/tex16.png"]
                  :user-data {"iGlobalBeatCount" (atom {:synth beats :tap "global-beat-count"})
                              "iBeat"            ibeat

                              "iStars" stars-weight
                              "iColor" color
                              "iCircleCount" circle-count
                              "iHalfPi" circle-slice
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
                              "iSmashTheParens" break-parens-in-case-of-emergency
                              })

  (comment
    (stop-graphics "resources/shaders/nyc.glsl")
    (stop-everything!)
    (stop))
  )
