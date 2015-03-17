(ns cassiopeia.emacs
  (:use [overtone.core]))

(defonce client (osc-client "localhost" 4558))

(dotimes [i 20]
  (Thread/sleep 5000)
  (osc-send client (choose ["/flip" "/flop"]) ""))
