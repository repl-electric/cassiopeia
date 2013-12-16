(ns stars.engine.sequencer
  (:use overtone.live)
  (:require [stars.engine.mixers :as mixers]))

(defsynth orig-mono-sequencer
  "Plays a single channel audio buffer (with panning)"
  [buf 0 rate 1 out-bus 0 beat-num 0 pattern 0  num-steps 8 beat-cnt-bus 0 beat-trg-bus 0 rq-bus 0]
  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 pattern cnt)
                      (= beat-num (mod cnt num-steps))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out out-bus (* vol
                    (pan2
                     (rlpf
                      (scaled-play-buf 1 buf rate bar-trg)
                      (demand bar-trg 0 (dbrown 200 20000 50 INF))
                      (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 pattern 0  num-steps 8 beat-cnt-bus 0 beat-trg-bus 0 rq-bus 0 pan 0]
                  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 pattern cnt)
                      (= beat-num (mod cnt num-steps))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out out-bus (* vol (pan2 (scaled-play-buf 1 buf rate bar-trg) pan)))))

(defn- start-synths [samples patterns mixers num-steps tgt-group beat-cnt-bus beat-trg-bus out-bus]
  (let [out-busses (if mixers
                     (map :in-bus mixers)
                     (repeat out-bus))]
    (doall (mapcat (fn [sample pattern out-bus]
                     (map (fn [step-idx]
                            (mono-sequencer [:tail tgt-group]
                                            :buf (to-sc-id sample)
                                            :beat-num step-idx
                                            :pattern (:pattern-buf pattern)
                                            :beat-cnt-bus beat-cnt-bus
                                            :beat-trg-bus beat-trg-bus
                                            :out-bus out-bus))
                          (range num-steps)))
                   samples
                   patterns
                   out-busses))))

(defn- mk-sequence-patterns
  "Setup our buffers"
  [samples num-steps]
  (doall (map (fn [sample]
                (with-meta {:num-steps num-steps
                            :pattern-buf (buffer num-steps)}
                  {:type ::sequence-pattern}))
              samples)))

(defn mk-sequencer [nk-group handle samples num-steps tgt-group beat-cnt-bus beat-trg-bus out-bus]
  (let [patterns (mk-sequence-patterns samples num-steps)
        container-group (group handle :tail tgt-group)
        seq-group       (group "lp-sequencer" :head container-group)
        mixer-group     (group "lp-mixers" :after seq-group)
        mixer-handles   (map #(str handle "-" %) (range (count samples)))
        mixers          (doall (map #(mixers/add-nk-mixer nk-group % mixer-group out-bus) mixer-handles))
        synths   (start-synths samples patterns mixers num-steps seq-group beat-cnt-bus beat-trg-bus out-bus)]
    (with-meta {:patterns patterns
                :num-steps num-steps
                :num-samples (count samples)
                :synths (agent synths)

                :seq-group seq-group
                :mixer-group mixer-group
                :mixer-handles mixer-handles
                :mixers mixers}
      {:type ::sequencer})))
