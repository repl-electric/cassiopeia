(ns cassiopeia.waves.buf-effects
  (:use overtone.live)
  (:require [cassiopeia.engine.timing :as time]))

(defsynth schroeder-reverb
  [rate 1 buf 0]
  (let [input    (pan2 (play-buf 1 buf rate :action FREE) -0.5)
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))]
    (out 0 output)))

(defsynth schroeder-reverb-mic
  [rate 1 dec 1 del 10 out-bus 0]
  (let [input    (pan2 (allpass-c (sound-in) 10  dec del))
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))]
    (out out-bus output)))

(defsynth phasor-skipping-sequencer
  "Supports looping and jumping position"
  [buf 0 rate 1 out-bus 0 start-point 0 bar-trg [0 :tr] loop? 0 amp 1.0 cb 0 end 0 start 0]
  (let [ph (phasor:ar :trig bar-trg
                      :rate (* rate (buf-rate-scale:kr buf))
                      :start start
                      :end end
                      :reset-pos start-point)
        br (buf-rd:ar 1 buf ph loop?)]
    (out:kr cb (a2k ph))
    (out out-bus (* amp br))))

(defsynth skipping-seqer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 pattern 0  num-steps 8 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) amp 0.7
   rate-start 0.1
   rate-limit 0.9
   start 0 end 0 start-point 0 cb 0]
  (let [cnt      (in:kr beat-bus)
        rander (mod cnt 1)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 pattern cnt)
                      (= beat-num (mod cnt num-steps))
                      beat-trg)
        vol      (set-reset-ff bar-trg)

        ph (phasor:ar :trig bar-trg
                      :rate (* rate (buf-rate-scale:kr buf))
                      :start start
                      :end end)
        br (buf-rd:ar 1 buf ph :loop false)
        ]
    (out:kr cb (a2k ph))
    (out out-bus (* vol amp br))))

(defsynth buf->perc-inst [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0
                          pattern-buf 0 pattern-size 3
                          amp-buf 0
                          duration-buf 0
                          voices 3
                          beat-count-bus 0]
  (let [cnt (in:kr beat-count-bus)
        dur (buf-rd:kr 1 duration-buf (mod cnt voices))
        cutom-amp (buf-rd:kr 1 amp-buf (mod cnt pattern-size))
        pos-frac (buf-rd:kr 1 pattern-buf (mod cnt pattern-size))

        bar-trg  (= beat-num (mod cnt voices))
        amp      (set-reset-ff bar-trg)

        width-frac (* (/ dur (buf-dur:ir buf)) rate)

        sig [(buf-rd:ar 1 buf (phasor:ar 0
                                         (* rate (buf-rate-scale:kr buf))
                                         (* pos-frac (buf-samples:kr buf))
                                         (+ (* pos-frac (buf-samples:kr buf))
                                            (* width-frac (buf-samples:kr buf))))
                        true
                        inter)]
        env (env-gen:kr (env-perc) bar-trg 1 0 dur)]
    (out:ar out-bus (pan2 (* env amp cutom-amp sig)))))


(defsynth buf->smooth-inst [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0
                            pattern-buf 0 pattern-size 3
                            amp-buf 0
                            duration-buf 0
                            voices 3
                            beat-count-bus 0
                            sin-dur 1]
  (let [cnt (in:kr beat-count-bus)
        dur (buf-rd:kr 1 duration-buf (mod cnt voices))
        custom-amp (buf-rd:kr 1 amp-buf (mod cnt pattern-size))
        pos-frac (buf-rd:kr 1 pattern-buf (mod cnt pattern-size))
        bar-trg  (= beat-num (mod cnt voices))
        amp      (set-reset-ff bar-trg)

        width-frac (* (/ dur 2 (buf-dur:ir buf)) rate)
        forward (buf-rd:ar 1 buf (phasor:ar 0
                                            (* (abs rate) (buf-rate-scale:kr buf))
                                            (- (* width-frac (buf-samples:kr buf))
                                               (* pos-frac (buf-samples:kr buf)))
                                            (+ (* width-frac (buf-samples:kr buf))
                                               (* pos-frac (buf-samples:kr buf))))
                           1
                           inter)

        backward (buf-rd:ar 1 buf (phasor:ar 0
                                             (* -1 (abs rate) (buf-rate-scale:kr buf))
                                             (+ (* width-frac (buf-samples:kr buf))
                                                (* pos-frac (buf-samples:kr buf)))
                                             (- (* width-frac (buf-samples:kr buf))
                                                (* pos-frac (buf-samples:kr buf))))
                            1
                            inter)

        sound (bi-pan-b2:ar forward backward (f-sin-osc:kr dur))
        env (env-gen:kr (env-sine sin-dur) bar-trg 1 0 dur)]
        (out out-bus (* env amp custom-amp sound))))


(defn lazy-nths [samples] (cons (rand-nth samples) (lazy-seq (lazy-nths samples))))

(defn sample->percussive
  ([samples] (sample->percussive samples 3))
  ([samples voices pattern-size]
     (let [g (group "percussive grouping")
           selected-samples (take voices (lazy-nths samples))
           fraction-buf (buffer voices)
           amp-buf      (buffer pattern-size)
           duration-buf (buffer pattern-size)
           synths (doall
                   (map
                    (fn [n]
                      (let [selected-buf (nth selected-samples n)]
                        (println (str "id:" n " buf:" (:name selected-buf)))
                        (buf->perc-inst
                         [:head g]
                         :buf selected-buf
                         :rate 1
                         :beat-num n
                         :pattern-size (buffer-size fraction-buf)
                         :pattern-buf fraction-buf
                         :amp-buf amp-buf
                         :duration-buf duration-buf
                         :voices voices
                         :beat-count-bus (:count time/beat-1th))))
                    (range 0 voices)))]
       (with-meta {:group g
                   :synths synths
                   :samples selected-samples
                   :fraction fraction-buf
                   :amp amp-buf
                   :duration duration-buf}
         {:type ::percussive-sample}))))

(defn sample->smooth
  ([samples] (sample->smooth samples 3 3))
  ([samples voices pattern-size] (sample->smooth samples voices pattern-size (take voices (lazy-nths samples))) )
  ([samples voices pattern-size selected-samples]
     (let [g (group "smooth grouping")
           fraction-buf (buffer voices)
           amp-buf      (buffer pattern-size)
           duration-buf (buffer pattern-size)
           synths (doall
                   (map
                    (fn [n]
                      (let [selected-buf (nth selected-samples n)]
                        (println (str "id:" n " buf:" (:name selected-buf)))
                        (buf->smooth-inst
                         [:head g]
                         :buf selected-buf
                         :rate 1
                         :beat-num n
                         :pattern-size (buffer-size fraction-buf)
                         :pattern-buf fraction-buf
                         :amp-buf amp-buf
                         :duration-buf duration-buf
                         :voices voices
                         :beat-count-bus (:count time/beat-1th))))
                    (range 0 voices)))]
       (with-meta {:group g
                   :synths synths
                   :samples selected-samples
                   :fraction fraction-buf
                   :amp amp-buf
                   :duration duration-buf}
         {:type ::smooth-sample}))))

(defn spin-for [voice durations thing]
  (buffer-write! thing voice [(rand-nth durations)]))

(comment
  (use 'cassiopeia.samples)
  (use 'cassiopeia.engine.core)
  (def voices 8)
  (def pattern-size (rand-nth [1 2 4 8 16 32 64 128 256]))
  (def durations [1/8 1/4 1/2 1])
  (def pattern-size 8)

  (kill buf->perc-inst)
  (kill buf->smooth-inst)

  (on-beat-trigger 64 #(do (spin-for (rand-int voices) durations (:duration ss))))
  (on-beat-trigger 128 (fn []
                         (do
                           (let [p (take pattern-size (repeatedly #(/ (rand 512) 512)))]
                             (println p)
                             (pattern! (:fraction ss) p)))))

  (remove-all-beat-triggers)

  (def ss (sample->smooth [rf-solve-s rf-full-s rf-theorems-s rf-full-s rf-fx-s] voices pattern-size))
  (pattern! (:duration ss) (take voices (repeatedly #(rand-nth durations))))
  (pattern! (:amp ss)      (take pattern-size (repeatedly #(ranged-rand 0.1 0.2))))
  (pattern! (:fraction ss) (take pattern-size (repeatedly #(/ (rand 512) 512))))

  (def example-samples
    [rf-full-s rf-full-s rf-solve-s rf-fx-s rf-solve-s rf-full-s rf-full-s rf-full-s]
    ;;[rf-full-s rf-theorems-s rf-full-s rf-full-s rf-theorems-s rf-fx-s rf-solve-s rf-full-s]
    )
  (def ss (sample->smooth [] voices pattern-size example-samples))
  (pattern! (:duration ss) [1/2 0 0 0 1/2 0 0 0])
  (pattern! (:duration ss) [1 0 0 0 1 0 0 0])
  (pattern! (:amp ss)      [0.1 0 0 0 0.15 0 0 0])
  ;;(pattern! (:amp ss)    [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0])
  (pattern! (:fraction ss) [0.8845941 0.3484526 0.02742675 0.82377213 0.7945769 0.772626 0.45249504 0.35252455])

  (pattern! (:fraction ss) [0.2470634 0.5662428 0.63178784 0.9357417 0.66654444 0.0969285 0.40005338 0.675227])

  ;; (ctl (:group ss) :sin-dur 1)

  (def gs (sample->percussive
           [rf-solve-s rf-full-s rf-theorems-s rf-full-s rf-fx-s] voices pattern-size))
  (buffer-write! (:duration gs) (take voices (repeatedly #(rand-nth durations))))
  (buffer-write! (:amp gs)      (take pattern-size (repeatedly #(ranged-rand 0.3 0.5))))
  (buffer-write! (:fraction gs) (take voices (repeatedly #(/ (rand 512) 512))))

  (pattern! (:duration gs) [1/3 1/4 1/2 1/2 1/4 0 1/4 1/4])
  (pattern! (:amp gs)      [0.7 0.4 0.4 0.4 0.3 0.3 0.5 0.5])
  (pattern! (:fraction gs)
            [0.70 0 0 0 0.1 0.9 0.9 0.50]
            [0 0 0 0 0 0 0 0 0]
            [0.9 0.9 0 0 0 0 0 0 0]
            [0.4 0.4 0.4 0.4 0.4 0.4 0.4 0.4 0.4]
            )
  (pattern! (:fraction gs) [1 0.9 0.1 0.1 0.1 0.1 0.1 0.1])

  )

(comment
  ;;id:0 buf:solve.wav
  ;;id:1 buf:fx.wav
  ;;id:2 buf:full.wav
  ;;id:3 buf:theorems.wav
  ;;id:4 buf:fx.wav
  ;;id:5 buf:fx.wav
  ;;id:6 buf:full.wav
  ;;id:7 buf:full.wav
)
