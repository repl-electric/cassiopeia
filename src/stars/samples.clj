(ns stars.samples
  (:use
   [overtone.live]))

(defonce orig-samples [(sample (freesound-path 777))   ;;kick
                       (sample (freesound-path 406))   ;;click
                       (sample (freesound-path 25649)) ;;subby
                       (sample (freesound-path 85291));;wop
                       ])

(defonce african-samples [(sample (freesound-path 127124))
                          (sample (freesound-path 173025))
                          (sample (freesound-path 178048))
                          (sample (freesound-path 21351))
                          (sample (freesound-path 21328))
                          (sample (freesound-path 21344))])

(defonce mouth-samples [(sample (freesound-path 34460))
                        (sample (freesound-path 20834))
                        (sample (freesound-path 16665))
                        (sample (freesound-path 62911))
                        (sample (freesound-path 18035))
                        (sample (freesound-path 2837))])

(defonce bass-samples [(sample (freesound-path 33637)) ;;boom
                       (sample (freesound-path 25649)) ;;subby
                       ])

(defonce transition-samples [(sample (freesound-path 127124))
                             (sample (freesound-path 25649))])

(defonce atmos-samples [(sample (freesound-path 2523))
                        (sample (freesound-path 18765))
                        (sample (freesound-path 48413))
                        (sample (freesound-path 64544))
                        (sample (freesound-path 116730))
                        (sample (freesound-path 113700))
                        (sample (freesound-path 113701))
                        (sample (freesound-path 113702))])

(defonce trigger-samples [(sample (freesound-path 86773))
                        (sample (freesound-path 77305))
                        (sample (freesound-path 102720))
                        (sample (freesound-path 46092))
                        (sample (freesound-path 135117))
                        (sample (freesound-path 57143))
                        (sample (freesound-path 85487))
                        (sample (freesound-path 70052))])

(defonce ambient-drum-samples [(sample (freesound-path 72989))
                               (sample (freesound-path 122048))
                               (sample (freesound-path 87726))
                               (sample (freesound-path 36325))])

(defonce atmossy {:stream-under-bridge (sample (freesound-path 117329))
                        :birdsong            (sample (freesound-path 18765))
                        :rain-with-thunder   (sample (freesound-path 2523))
                        :ocean-waves         (sample (freesound-path 48412))
                        :water-dripping      (sample (freesound-path 116730))
                        :bubbles1            (sample (freesound-path 113700))
                        :bubbles2            (sample (freesound-path 113701))
                        :bubbles3            (sample (freesound-path 113702))})

(defonce bleep-samples
  [(freesound 34205)
   (freesound 25882)
   (freesound 74233)
   (freesound 70106)
   (freesound 64072)])

(defonce bleep2-samples
  [(freesound 64072)
   (freesound 74233)
   (freesound 25882)
   (freesound 34205)
   (freesound 70106)
   (freesound 64072)])

(defonce bleep1-samples
  [(freesound 70106)
   (freesound 25882)
   (freesound 34205)
   (freesound 74233)
   (freesound 64072)])

(defonce clapkick1-samples
  [(freesound 47452)
   (freesound 47453)
   (freesound 47454)
   (freesound 47450)
   (freesound 47451)])

(defonce clapkick2-samples
  [(freesound 47457)
   (freesound 47456)
   (freesound 47455)
   (freesound 47449)
   (freesound 47448)])

(defonce kicks-samples
  [(freesound 147483)
   (freesound 147482)
   (freesound 147480)
   (freesound 147479)
   (freesound 147478)])
