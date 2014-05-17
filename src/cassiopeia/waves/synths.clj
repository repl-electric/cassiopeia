(ns cassiopeia.waves.synths
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.mixers :as mix]
            [overtone.inst.synth :as s]))

(defsynth spacy [buf 0 chain 0 amp 1]
  (let [in  (play-buf:ar 1 buf (buf-rate-scale:kr buf) :loop false)
        b (local-buf 2048)
        chain  (fft b in)
        chain  (pv-rect-comb chain 8 0.6 0.6)
        src (* amp (* 0.5  (ifft chain)))]
    (out 0 [src src])))

(defsynth echoey-buf
  "Play an existing buffer with a heavy echo"
  [b 0 frames [256 :ir] out-bus 0 thresh 0.07]
  (let [in (play-buf 1 b (* (buf-rate-scale:kr b) 1.1))
        chain (fft (local-buf frames) in)
        chain (pv-mag-freeze chain -0.1)
        output (* (ifft chain) 0.9)
        output (+ output (comb-c:ar output 1 0.3 6))]
    (out out-bus output)))

(defsynth seqer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 pattern 0  num-steps 8 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) amp 0.7]
  (let [cnt      (in:kr beat-bus)
        rander (mod cnt 1)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 pattern cnt)
                      (= beat-num (mod cnt num-steps))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out out-bus (* vol amp (scaled-play-buf :num-channels 1
                                             :buf-num buf
                                             :rate (t-rand:kr 0.5 0.9 rander)
                                             :trigger bar-trg)))))

(defsynth rise-fall-pad
  [freq 440 t 4 amt 0.3 amp 0.8 out-bus 0 note-buf 0 seq-buf 0 beat-bus 0 beat-trg-bus 0 num-steps 16 beat-num 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)
        freq (midicps note)

        f-env      (env-gen (perc t t) 1 1 0 1)
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 4 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 0.1 (* 2 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.9)]
    (out out-bus (pan2 (* amp echo)))))

(defsynth quick-kick
  [freq {:default 20.0 :min 20 :max 400 :step 1}
   attack {:default 0.0001 :min 0.00001 :max 2 :step 0.0001}
   decay  {:default 0.374 :min 0.00001 :max 2 :step 0.0001}
   fattack {:default 0.001 :min 0.00001 :max 2 :step 0.0001}
   fdecay {:default 0.282 :min 0.00001 :max 2 :step 0.0001}
   amp {:default 0.8 :min 0.01 :max 1 :step 0.01}
   beat-bus 0
   beat-trg-bus 0
   note-buf 0
   num-steps 0
   beat-num 0
   seq-buf 0
   out-bus 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)

        freq-env (env-gen:kr (perc fattack fdecay) :gate bar-trg)
        wave (sin-osc (+ (* 0.4 freq) (* 14 freq freq-env)))
        env  (env-gen (perc :release 0.04 :attack 0.04) :gate bar-trg)
        src (* env wave)
        dist (clip2 (tanh (* 0.5 (distort (* 1.5 src)))) 0.7)
        eq (b-peak-eq dist 50.41 1 44)]
    (out out-bus (* amp env eq))))

(defsynth buffer->tap [beat-buf 0 beat-bus 0 beat-size 16 measure 6]
  (let [cnt (in:kr beat-bus)
        beat (buf-rd:kr 1 beat-buf cnt)
        _  (tap "beat"          60 (a2k beat))
        _  (tap "beat-count"    60 (a2k (mod cnt beat-size)))
        _  (tap "measure-count" 60 (a2k (/ (mod cnt (* measure beat-size)) measure)))])
  (out 0 0))

(defsynth kick2
  "We take the sting out of the overtone kick2 drum giving a softer more mellow kick"
  [amp 0.8 mod-freq  5 mod-index 5 sustain 0.4 noise 0.025
   beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) note-buf 0 seq-buf 0 beat-num 0 num-steps 8 out-bus 0]
  (let [cnt      (mod (in:kr beat-bus) num-steps)
        beat-trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 note-buf beat-num)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num cnt)
                     beat-trg)
        freq (midicps note)

        pitch-contour (line:kr (* 2 freq) freq 0.02)
        drum (lpf (sin-osc pitch-contour (sin-osc mod-freq (/ mod-index 1.3))) 1000)
        drum-env (env-gen (perc 0.005 sustain) :gate bar-trg)
        hit (hpf (* noise (white-noise)) 500)
        hit (lpf hit (line 6000 500 0.03))
        hit-env (env-gen (perc) :gate bar-trg)
        src (* amp (+ (* drum drum-env) (* hit hit-env)))]
    (out out-bus (pan2 src))))

(defsynth shrill-pulsar [note-buf 0 beat-bus 0 beat-trg-bus 0 size 1 r 0 amp 1]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trig (and (> note 0) trg)
        vol (set-reset-ff gate-trig)

        freq (midicps note)
        src (+ [(lpf (saw freq) 500)] [(sin-osc (* 1.01 freq))])
        src (rlpf src 1200 0.3)
        src (free-verb src)
        e (env-gen (adsr :release 1) :gate gate-trig)]
    (out 0 (pan2:ar (* vol amp e src)))))

(defsynth growl [note-buf 0 beat-bus 0 beat-trg-bus 0 amp 1 pan-rate 50]
  (let [cnt (in:kr beat-bus)
        trg (in:kr beat-trg-bus)
        note (buf-rd:kr 1 note-buf cnt)
        freq (midicps note)
        vol (> note 0)

        e (env-gen (perc :attack 10 :sustain 1 :release 1) :gate trg)
        src (lpf (mix [(saw (* 0.25 freq))
                       (sin-osc (* 1.01 freq))]))
        src (pitch-shift src 0.4 1 0 0.01)
;;        panning (+ 0.1 (sin-osc:kr pan-rate))
        src (pan2:ar (* vol amp e src))
;;        _ (tap "g" 60 (a2k src))
        ]
    (out 0 src)))

(defsynth glass-ping [out-bus 0 velocity 80 t 0.6 amp 1 seq-buf 0 note-buf 0 beat-trg-bus 0 beat-bus 0 num-steps 8 beat-num 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)

        freq     (midicps note)
        sub-freq (midicps (- note 12))
        velocity (/ velocity 127.0)
        sawz1    (* 0.275 (saw [freq (* 1.01 freq)]))
        sawz2    (* 0.75 (saw [(- freq 2) (+ 1 freq)]))
        sqz      (* 0.3 (pulse [sub-freq (- sub-freq 1)]))
        mixed    (* 5 (+ sawz1 sawz2 sqz))
        env      (env-gen (adsr 0.1 3.3 0.6 1) :gate bar-trg)
        filt     (*  (moog-ff mixed (* velocity (+ freq 200)) 2.2 bar-trg))]
    (out out-bus (* amp env filt))))

(defsynth dark-ambience [out-bus 0 amp 1 mul 0.2 room-size 70 rev-time 99 freq 60 ring-mul 55]
  (let [pink (hpf:ar (* (* 0.005 (pink-noise)) (line:kr 0 1 9)) 5)
        src1 (ringz (* pink (lf-noise1:kr 0.15)) (+ freq (* ring-mul 0)) mul)
        src2 (ringz (* pink (lf-noise1:kr 0.15)) (+ freq (* ring-mul 1)) mul)
        src3 (ringz (* pink (lf-noise1:kr 0.15)) (+ freq (* ring-mul 2)) mul)
        src (tanh (g-verb (sum [src1 src2 src3]) room-size rev-time))]
    (out out-bus (* amp src))))

(defsynth whitenoise-hat [out-bus 0 seq-buf 0 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) num-steps 0 beat-num 0 amp 1]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)
        w (* 0.01 (white-noise:ar))
        e (env-gen (perc :attack 0 :release 1) :gate bar-trg)]
    (out out-bus (pan2 (* amp e w)))))

(defsynth high-hats [out-bus 0 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) note-buf 0 seq-buf 0 beat-num 0 num-steps 0
                     attack 0.001 release 0.1 mix 0 room 0 damp 0 amp 1]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     (not= note 0)
                     beat-trg)

        freq (midicps note)
        c (pm-osc:ar freq (* freq (t-rand 0.25 2.0 bar-trg)) (t-rand 0.1 (* 2 Math/PI) bar-trg))
        e (env-gen:kr (env-perc attack release) bar-trg)
        src (/ (* c e 0.125) 2)
        src (free-verb src :mix mix :room room :damp damp)]
    (out out-bus [(* amp src) (* amp src)])))

(defsynth pulsar [note-buf 0 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) amp 1]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trg (and (> note 0) trg)

        vol (set-reset-ff gate-trg)
        freq (midicps note)
        src (lpf (saw freq) 400)
        e (env-gen (perc) :gate gate-trg)]
    (out 0 (pan2:ar (* vol amp e src) (sin-osc:kr 2)))))

(defsynth growler [note-buf 0 beat-bus 0 beat-trg-bus 0 amp 1 attack 10 sustain 2 release 2]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trig (and (> note 0) trg)
        vol (set-reset-ff gate-trig)

        freq (midicps note)
        e (env-gen (perc attack release sustain) :gate gate-trig)
        src (mix [(sin-osc (* 1.01 freq))
                  (sin-osc freq)
                  (lpf (saw (* 0.99 freq)))])
        src (pitch-shift src 0.4 1 0 0.01)
        src (free-verb src :room 10)]
    (out 0 (pan2:ar (* vol amp e src)))))

(defsynth shrill-pong [out-bus 0 velocity 80 t 0.6 amp 1 seq-buf 0 note-buf 0
                       beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat)
                       duration-bus 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        duration (buf-rd:kr 1 duration-bus cnt)
        bar-trg (and (> note 0) beat-trg)
        freq (midicps note)

        vol (set-reset-ff bar-trg)

        src (+ [(sin-osc (* 1.01 freq))
                (rlpf (saw freq))
                (rlpf (pulse freq) 1200)])
        src (free-verb src :room 10)
;;        _ (tap "a" 60 (a2k src))
        e (env-gen (adsr :release 4 :sustain 4 :attack 0.6 :curve -1) :gate beat-trg :time-scale duration)]
    (out out-bus (* vol amp e src))))

(defsynth fizzy-pulsar [note-buf 0 beat-bus (:count time/main-beat) beat-trg-bus (:beat time/main-beat) size 1 r 0 amp 1 duration-bus 0]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trig (and (> note 0) trg)
        vol (set-reset-ff gate-trig)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (and (not= durs 0) (t-duty:kr (dseq durs INFINITE)))
        freq (demand:kr trig 0 (drand note INFINITE))
        freq (midicps freq)

        src (+ [(sin-osc freq)
                (lpf (saw freq) 1200)
                (lpf (pulse freq) 1200)])
        src (lag src 0.005)
        e (env-gen (adsr :release 2 :sustain 2 :attack 0.5) :gate trg :time-scale durs)]
    (out 0 (pan2:ar (* vol amp e src)))))

(defsynth deep-space-signals [out-bus 0 freq 300]
  (let [src1 (sin-osc:ar (repeatedly 8  #(ranged-rand 300 1000)))
        src2 (* 0.2 (lf-pulse:kr (repeatedly 8 #(ranged-rand 0.1 4)) 0 0.1))
        src (splay:ar (* src1 src2))
        src (* src (lf-tri:ar 0.01))
        src (g-verb:ar src)]
    (out out-bus src)))

(comment
  (deep-space-signals)
  (kill deep-space-signals))

(def freq-limit-buf (buffer 12))
(defsynth space-ping [amp 1 freq-limit-buf 0 beat-bus 0 dark-freq 1]
  (let [cnt (in:kr beat-bus)
        freq (buf-rd:kr 1 freq-limit-buf cnt)

        src (bpf:ar
             (* (repeat 12 1) dark-freq (pink-noise:ar))
             (round (lin-exp (lf-noise0:kr (* freq (repeat 12 1))) -1 1 99 600) 50)
             2e-3)
        src (splay:ar src)
        src (* 25 src)]
    (out 0 (* amp src))))

(comment
  (def space-p (space-ping :freq-limit-buf freq-limit-buf :beat-bus (:beat time/main-beat) :amp 2))
  (kill space-p)
  (ctl space-p :amp 5)
  (ctl time/root-s :rate 4)
  (ctl p :dark-freq 1)
  (buffer-write! freq-limit-buf (flatten (repeat 3 [5.9 5.9 0.5 0.5]))))

(comment
  (defsynth sparkling-darkness [freq 220 amp 3]
    (out 0 (pan2 (* amp (bpf:ar (* 1 (pink-noise:ar)) freq 2e-3)) (sin-osc:kr 1) 1))))

(comment
  (def s (playz))
  (ctl s :freq (midi->hz (note :A3))))


;;Experimental Synths

(comment

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
      (out out-bus (* amp (pan2 src panning 1.0))))))

(defsynth tick [freq 880 dur 0.1 level 0.25 pan 0.0 out-bus 0]
  (let [amp (env-gen (env-perc) :action FREE :time-scale dur)
        snd (lpf:ar (white-noise:ar) freq)]
    (out out-bus (pan2:ar (* snd amp level) pan))))
