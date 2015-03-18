(ns cassiopeia.emacs
  (:use [overtone.core] [mud.core] [mud.timing] [cassiopeia.samples]))

(defonce client (osc-client "localhost" 4558))

(dotimes [i 100]
  (Thread/sleep 250)
  (kick-s)
  (osc-send client "/beat" ""))
