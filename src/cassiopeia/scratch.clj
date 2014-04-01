(ns cassiopeia.scratch
  (:use overtone.live))

(defsynth woosh [freq 970 out-bus 0 swish 970 amp 0.1]
  (let [whoosh (lag
                (env-gen:kr (envelope [0.8 1.0 0.9 0.7 0.5 0.3 0.24 0.12 0.0]
                                      [0.08 0.2 0.15 0.11 0.08 0.09 0.06 0.18]
                                      2)
                            :time-scale (* 1.3 1.5))
                0.2)
        woosh-vol (env-gen:kr (envelope [0.0, 0.2, 0.7, 0.9, 1.0, 0.8, 0.6, 0.43, 0.22, 0.0]
                                        [0.4, 0.5, 0.2, 0.15, 0.17, 0.11, 0.1, 0.16, 0.28]
                                        2)
                              :time-scale (* 0.9 1.5))

        woosh-vol (* (* 1.5 (lag woosh-vol 0.2)) (lag (+ 1 (* 0.2 (lf-noise1:kr 37.2))) 0.2))
        noise (mix (comb-c:ar (* 0.2 (white-noise:ar)) 0.1 [(lin-exp whoosh 0.16 0.00011)
                                                            (lin-exp whoosh 0.19 0.00013)] 0.09))
        noise (+ noise (brown-noise))

        swish (* swish (+ 1 (* 0.2 (lf-noise0:kr 365))))
        swish (* swish (range-lin whoosh 0.8 1.3))

        swish (mix
               [(* 0.4 (sin-osc:ar (+ swish (* 0 (lin-exp whoosh 45 1))) (ranged-rand 0.0 1.0)))
                (* 0.4 (sin-osc:ar (+ swish (* 1 (lin-exp whoosh 45 1))) (ranged-rand 0.0 1.0)))
                (* 0.4 (sin-osc:ar (+ swish (* 2 (lin-exp whoosh 45 1))) (ranged-rand 0.0 1.0)))])
        src (+ noise swish)
        src (* src woosh-vol)
        panning (line:kr -0.7 0.7 3)]
    (out out-bus (pan2 src panning 1.0))))

(woosh)
(kill woosh)


(defonce silent-buffer (buffer 0))
(defonce soprano-samples (doall (map (fn [idx note] [note (load-sample (str (format "~/soprano/vor_sopr_sustain_ah_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(defonce index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defsynth sing [note 60 amp 0.1]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr :release 2))]
    (out 0 (pan2 (* amp env (play-buf:ar 1 buf))))))
