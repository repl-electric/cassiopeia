(ns cassiopeia.scratch
  (:use overtone.live))

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

(defonce silent-buffer (buffer 0))
(defonce soprano-samples (doall (map (fn [idx note] [note (load-sample (str (format "~/soprano/vor_sopr_sustain_ah_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(defonce index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defsynth sing [out-bus 0 note 60 amp 0.1 pos 0]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr :attack 2 :release 1 :sustain 1 :decay 0.2) :action FREE )]
    (out 0 (* env amp (pan2 (scaled-play-buf 1 buf) pos)))))

(comment
  (sing :note 61 :amp 0.2 :pos -1)
  (sing :note 60 :amp 0.2 :pos 1))
