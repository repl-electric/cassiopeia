(ns cassiopeia.destination.ruchbah
  "
 ######
 #     # #    #  ####  #    # #####    ##   #    #
 #     # #    # #    # #    # #    #  #  #  #    #
 ######  #    # #      ###### #####  #    # ######
 #   #   #    # #      #    # #    # ###### #    #
 #    #  #    # #    # #    # #    # #    # #    #
 #     #  ####   ####  #    # #####  #    # #    #

 An Algol-type eclipsing variable star.
 It appears to have a blue-white hue and it is 99 light-years from Earth.
"
  (:use overtone.live)
  (:use cassiopeia.samples)
  (:use cassiopeia.warm-up)
  (:require [cassiopeia.engine.timing :as timing]
            [overtone.inst.synth :as s]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.sequencer :as sequencer]
            [cassiopeia.engine.mixers :as mix]
            [cassiopeia.engine.scheduled-sampler :as scheduled-sampler]
            [cassiopeia.data.ruchbah :as data]
            [cassiopeia.engine.monome-sequencer :as mon-seq]))

;;;;;;;;;;;;;;;;;;;
;; Instruments   ;;
;;;;;;;;;;;;;;;;;;;

(do

(defonce melody-duration-b (buffer 128))
(defonce melody-notes-b    (buffer 128))

(defsynth melody [duration-bus 0 room 0.5 damp 0.5 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0 pitch-dis 0 time-dis 0]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        env (env-gen:ar (env-asr :release 0.25 :sustain 0.8) trig)
        src (* 0.3 (lf-tri:ar freq))
        src (pitch-shift src 0.9 0.9 pitch-dis time-dis)]
    (out:ar out-bus (* amp env (pan2 src (t-rand:kr -1 1 trig))))))

(comment
  (def m (melody :duration-bus melody-duration-b :offset-bus melody-notes-b
               :beat-count-bus (:count timing/beat-1th) :amp 1))

  (ctl m :pitch-dis 0.01 :time-dis 0.01 :amp 0)

  (ctl m :pitch-dis 0.01 :time-dis 0.01 :amp 1.4)

  (ctl m :amp 0)
  (kill melody)

  (stop))

(def flow-buf (buffer 128))
(def flow-f-buf (buffer 128))
(def flow-dur-buf (buffer 128))

(do
  (defsynth overpad
    [out-bus 0 amp 0.7 attack 0.001 release 2 note-buf 0 beat-count-bus 0 beat-trg-bus 0
     dur-buf 0
     tonal 0.99 bass-thrust 0.7 fizzing 3]
    (let [cnt  (in:kr beat-count-bus)
          durs (buf-rd:kr 1 dur-buf cnt)
          trig  (in:kr beat-trg-bus)
          note (buf-rd:kr 1 note-buf cnt)
          freq (midicps note)

          env   (env-gen (perc attack release) trig)
          f-env (+ freq (* fizzing freq (env-gen (perc 0.012 (- release 0.1)) trig)))
          bfreq (/ freq 2)
          sig   (apply +
                       (concat (* bass-thrust (sin-osc [bfreq (* tonal bfreq)]))
                               (lpf (saw [freq (* freq 1.01)]) f-env)))
          audio (* amp env sig)]
      (out out-bus (pan2 audio))))

  (comment
    (def o (overpad :release 16
                    :note-buf flow-f-buf
                    :beat-count-bus (:count timing/beat-2th)
                    :beat-trig-bus (:beat timing/beat-2th)
                    :amp 0.5
                    :attack 5))

    (buffer-write! flow-f-buf
                   (take 128
                         (cycle
                          (map note [:A3 :A3 :E3 :E3 :D3 :D3 :C4 :C4 :A3 :D3
                                     :A3 :E3 :D3 :C3 :A3 :E3 :D3 :C3
                                     ]))))))

(defsynth tb303
  [note-buf 0
   beat-count-bus 0
   beat-trg-bus 0
   wave       {:default 1 :min 0 :max 2 :step 1}
   r          {:default 0.8 :min 0.01 :max 0.99 :step 0.01}
   attack     {:default 0.01 :min 0.001 :max 4 :step 0.001}
   decay      {:default 0.1 :min 0.001 :max 4 :step 0.001}
   sustain    {:default 0.6 :min 0.001 :max 0.99 :step 0.001}
   release    {:default 0.01 :min 0.001 :max 4 :step 0.001}
   cutoff     {:default 100 :min 1 :max 20000 :step 1}
   env-amount {:default 0.01 :min 0.001 :max 4 :step 0.001}
   amp        {:default 0.5 :min 0 :max 15 :step 0.01}
   out-bus    0]
  (let [cnt (in:kr beat-count-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg  (in:kr beat-trg-bus)

        freq       (midicps note)
        freqs      [freq (* 1.01 freq)]
        vol-env    (env-gen (adsr attack decay sustain release) :gate trg)
        fil-env    (env-gen (perc) :gate trg)
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves      (* vol-env
                      [(saw freqs)
                       (pulse freqs 0.5)
                       (lf-tri freqs)])
        selector   (select wave waves)
        filt       (rlpf selector fil-cutoff r)
        src (* amp 100 filt)]
    (out out-bus [src src])))

(comment
  (kill tb))

(def bass-notes-buf (buffer 8))
(def phase-bass-buf (buffer 8))

(defsynth vintage-bass
  [out-bus 0 velocity 80 t 0.6 amp 1 seq-buf 0 note-buf 0 beat-trg-bus 0 beat-bus 0 num-steps 8 beat-num 0]
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
        env      (env-gen (adsr 0.1 3.3 0.4 0.8) :gate bar-trg)
        filt     (*  (moog-ff mixed (* velocity (+ freq 200)) 2.2 bar-trg))]
    (out out-bus (* amp env filt))))

(def bazz-g (group "bazz group"))
(defsynth bazz [out-bus 0 beat-bus 0 beat-trg-bus 0 note-buf 0 seq-buf 0 beat-num 0 num-steps 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)

        freq (t-rand 50 1300 bar-trg)
        c (pm-osc:ar freq (* freq (t-rand 0.25 2.0 bar-trg)) (t-rand 0.1 (* 2 Math/PI) bar-trg))
        e (env-gen:kr (env-perc 0.001 0.1) bar-trg)
        src (/ (* c e 0.125) 2)]
    (out out-bus [src src])))

(defsynth flek []
  (let [freq 550
        e (env-gen:kr (env-perc 0.001 4.0) :action FREE)
        c [(pulse:ar (+ (ranged-rand 1.0 10.0) (* freq (+ 0 1))) (lfd-noise1 (ranged-rand 1 10)))
           (pulse:ar (+ (ranged-rand 1.0 10.0) (* freq (+ 1 1))) (lfd-noise1 (ranged-rand 1 10)))
           (pulse:ar (+ (ranged-rand 1.0 10.0) (* freq (+ 2 1))) (lfd-noise1 (ranged-rand 1 10)))]]
    (out [0 1 20] (/ (* c e 0.125) 2))))

(comment
  (flek))

(definst v-bass
  [velocity 80 t 0.6 amp 1 seq-buf 0 note-buf 0 beat-trg-bus 0 beat-bus 0 num-steps 8 beat-num 0]
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
        env      (env-gen (adsr 0.1 3.3 0.4 0.8) :gate bar-trg)
        filt     (*  (moog-ff mixed (* velocity (+ freq 200)) 2.2 bar-trg))]
        (* amp env filt)))

(def v-bass-buf (buffer 128))

(defsynth dub-kick [out-bus 0 freq 80 beat-bus 0 beat-trg-bus 0 note-buf 0 num-steps 8 seq-buf 0 beat-num 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)

        cutoff-env (perc 0.001 1 freq -20)
        amp-env (perc 0.001 1 1 -8)
        osc-env (perc 0.001 1 freq -8)
        noiz (lpf (white-noise) (+ (env-gen:kr cutoff-env :gate bar-trg) 20))
        snd  (lpf (sin-osc (+ (env-gen:kr osc-env :gate bar-trg) 20)) 200)
        mixed (* (+ noiz snd) (env-gen amp-env :gate bar-trg))]
    (out out-bus [mixed mixed])))


(def moo-buf (buffer 128))
(def moo-amp-buf (buffer 128))

(defsynth moogey
  "Choose 0, 1, or 2 for saw, sin, or pulse"
  [amp  {:default 0.3 :min 0 :max 1 :step 0.01}
   osc1 {:default 0 :min 0 :max 2 :step 1}
   osc2 {:default 1 :min 0 :max 2 :step 1}
   osc1-level {:default 0.5 :min 0 :max 1 :step 0.01}
   osc2-level {:default 0 :min 0 :max 1 :step 0.01}
   cutoff {:default 500 :min 0 :max 20000 :step 1}
   attack {:default 0.0001 :min 0.0001 :max 5 :step 0.001}
   decay {:default 0.3 :min 0.0001 :max 5 :step 0.001}
   sustain {:default 0.99 :min 0.0001 :max 1 :step 0.001}
   release {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fattack {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fdecay {:default 0.3 :min 0.0001 :max 6 :step 0.001}
   fsustain {:default 0.999 :min 0.0001 :max 1 :step 0.001}
   frelease {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   beat-count-bus 0
   note-buf 0
   beat-trg-bus 0
   gate 1
   pants 0]
  (let [cnt (in:kr beat-count-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg  (in:kr beat-trg-bus)
        vol (buf-rd:kr 1 moo-amp-buf cnt)
        bar-trg  trg
        freq (midicps note)
        osc-bank-1 [(saw freq) (sin-osc freq) (pulse freq)]
        osc-bank-2 [(saw freq) (sin-osc freq) (pulse freq)]
        amp-env    (env-gen (adsr attack decay sustain release)     bar-trg)
        f-env      (env-gen (adsr fattack fdecay fsustain frelease) bar-trg)
        s1         (* osc1-level (select osc1 osc-bank-1))
        s2         (* osc2-level (select osc2 osc-bank-2))
        filt       (moog-ff (+ s1 s2) (* cutoff f-env) 3)]
    (out 0  [(* vol amp filt) (* vol amp filt)])))
)

;;;;;;;;;;;;;;;;;
;; Experiments ;;
;;;;;;;;;;;;;;;;;

(comment
  (def moo (s/mooger (note :A3)))
  (ctl moo :note (note :A2) :attack 16)

  (stop)
  (kill moo)

  (s/cs80lead :freq 90 :amp 0.2)
  (kill s/cs80lead)

  (s/overpad :note 60 :release 16)
  (kill s/overpad)

  (doseq [i (range 0 9)]
    (vintage-bass :amp 0.4 :note-buf bass-notes-buf
                  :seq-buf phase-bass-buf
                  :beat-bus (:count timing/beat-1th)
                  :beat-trg-bus (:beat timing/beat-1th) :num-steps 8 :beat-num i))

  (kill vintage-bass)
)

;;;;;;;;;;;
;; Score ;;
;;;;;;;;;;;

(def moo (moogey :note-buf moo-buf
                 :beat-count-bus (:count timing/beat-2th)
                 :beat-trig-bus (:beat timing/beat-2th)
                 :amp 0
                 :out-bus (mix/nkmx :r0)))

(buffer-write! moo-amp-buf
    (take 128 (cycle (flatten (concat
                               (map note [0 0 0 0 0 0 0 0
                                          0 0 0 0 0 0 0 0
                                          0 0 0 0 0 0 0 0
                                          1 1 1 1 1 1 1 1
                                          1 1 1 1 1 1 1 1]))))))

(buffer-write! moo-buf
  (take 128 (cycle (flatten (concat
    (map note [:A3 :E3 :D3 :C3 :D3 :E3 :A3 :C3 :A3 :E3 :D3 :C3 :D3 :E3 :A3 :C3]))))))

(ctl moo
     :fdecay 6
     :fsustain 2
     :frelease 1
     :fattack 4 :attack 0.1 :release 0.1 :decay 0.1 :sustain 1 :cutoff 400 :amp 3)

(ctl moo :cutoff 3000)
(ctl moo :amp 0)

(kill moogey)

(def tb (tb303 :attack 4
               :amp 0
               :sustain 0
               :decay 4
               :note-buf flow-buf
               :release 1
               :waves 3
               :env-amount 10
               :beat-count-bus (:count timing/beat-2th)
               :beat-trg-bus   (:beat timing/beat-2th)
               :out-bus (mix/nkmx :m0)))

(ctl tb :amp 0.6)
(ctl tb :amp 0)

(comment
  (ctl tb :amp 1.5)
  (ctl tb :env-amount 0.01 :attack 1 :waves 3 :sustain 0.6 :release 2))

(def o (overpad :note-buf flow-f-buf
                :beat-count-bus (:count timing/beat-2th)
                :beat-trig-bus (:beat timing/beat-2th)
                :amp 0
                :out-bus (mix/nkmx :r0)
                :release 1
                :attack 0))

(ctl o :amp 0.6)
(ctl o :amp 0)
(ctl o :release 1 :attack 0 :amp 0.2)


(buffer-write! flow-buf
               (take 128
                     (cycle
                      (map note (shuffle [:A3 :E3 :D3 :C4 :A3 :E3 :D3 :C3
                                          :A3 :E3 :D3 :C3 :A3 :E3 :D3 :C3
                                          ])))))

(buffer-write! flow-f-buf
                (take 128
                     (cycle
                      (map note (shuffle [:A3 :E3 :D3 :C4 :A3 :E3 :D3 :C3])))))

(buffer-write! flow-f-buf
  (take 128 (map note (cycle (flatten (concat (repeat 3 [:A3 :E3 :D3 :C3 :D3 :E3 :A3 :C3])
                                              [:A2 :E2 :D2 :C2 :D2 :E2 :A2 :C2]
                                              [:A2 :E2 :D2 :C2 :D2 :E2 :A2 :C2]))))))

(buffer-write! flow-buf   (take 128 (cycle (map note data/flow-buf-record))))
(buffer-write! flow-f-buf (take 128 (cycle (map note data/flow-f-buf-record))))
(buffer-write! flow-buf   (take 128 (cycle (map note data/flow-f-buf-record))))

;;(doall (map #(print (str (find-note-name (int (buffer-get flow-f-buf %)))) " ") (range 0 128)))

(comment
  (stop)

  (kill m))

(buffer-write! melody-duration-b (take 128 (cycle [1/2 1/4 1/4 1/2])))
(buffer-write! melody-notes-b (take 128 (cycle (map note [:A3 :A4 :B4 :C4]))))
(buffer-write! melody-duration-b (take 128 (cycle [1/8 1/4 1/8 1/4
                                                   1/8 1/8 1/4 1/8])))

(buffer-write! melody-notes-b (take 128 (cycle (map #(+ 0 (note %)) [:A3 :A4 :B4 :C4
                                                                     :c4 :B4 :A4 :A3]))))

(s/rise-fall-pad :freq (midi->hz (note :A3)))
(kill s/rise-fall-pad)

(def drum-samples-set [kick-s clap2-s snare-s hip-hop-kick-s])
(mon-seq/swap-samples! seq128 drum-samples-set)

(scheduled-sampler/schedule-sample boom-s timing/main-beat :mod-size 16)

(mon-seq/sequencer-write! seq128 0 [1 0 1 0 1 1 0 0])
(mon-seq/sequencer-write! seq128 1 [0 0 0 0 0 0 0 1])
(mon-seq/sequencer-write! seq128 2 [0 1 0 0 0 0 1 0])
(mon-seq/sequencer-write! seq128 3 [1 1 1 1 1 1 1 1])

(comment
  (mon-seq/sequencer-write! seq128 0 [0 0 0 0 0 0 0 0])
  (mon-seq/sequencer-write! seq128 1 [0 0 0 0 0 0 0 0])
  (mon-seq/sequencer-write! seq128 2 [0 0 0 0 0 0 0 0])
  (mon-seq/sequencer-write! seq128 3 [0 0 0 0 0 0 0 0])
)

(buffer-write! bass-notes-buf  (take 8 (cycle (map note [:E2]))))
(buffer-write! bass-notes-buf  (take 8 (cycle (map note [:D3 :E2]))))

(buffer-write! phase-bass-buf  [1 1 0 0 1 1 0 0])
(buffer-write! phase-bass-buf  [1 1 0 0 0 1 1 0])
(buffer-write! phase-bass-buf  [1 0 1 0 1 0 1 0])

(doseq [i (range 0 9)]
  (bazz
   [:head bazz-g]
   :amp 0.4 :note-buf bass-notes-buf
   :seq-buf phase-bass-buf
   :beat-bus (:count timing/beat-1th)
   :beat-trg-bus (:beat timing/beat-1th) :num-steps 8 :beat-num i))

(kill bazz)

(def dub-kick-g (group "dub kick group"))
(def dubkicks (doall (map
                      #(dub-kick
                       [:head dub-kick-g]
                       :amp 0.4
                       :freq (ranged-rand 80 110)
                       :note-buf bass-notes-buf
                       :seq-buf v-bass-buf
                       :freq (ranged-rand 80 100)
                       :beat-bus (:count timing/beat-1th)
                       :beat-trg-bus (:beat timing/beat-1th) :num-steps 18 :beat-num %) (range 0 18) ) ))

(ctl o :fizzing 10)
(ctl o :bass-thrust 4)
(ctl o :tonal 4)
(ctl o :bass-thrust 10)
(ctl o :amp 0)
(ctl dub-kick-g :freq 200)
(ctl dub-kick-g :freq 160)
(ctl dub-kick-g :freq 100)

(ctl timing/root-s :rate 2)

(ctl dub-kick-g :beat-bus (:beat timing/beat-1th) :beat-trg-bus (:count timing/beat-1th))
(ctl bazz-g :beat-bus (:beat timing/beat-1th) :beat-trg-bus (:count timing/beat-1th))

(ctl dub-kick-g :beat-bus (:beat timing/beat-1th) :beat-trg-bus (:count timing/beat-1th))
(ctl bazz-g :beat-bus (:beat timing/beat-1th) :beat-trg-bus (:count timing/beat-1th))

(kill dub-kick)

(buffer-write! v-bass-buf  (take 128 (cycle [1 1 0 1 1 0 1 1
                                             0 0 1 1 0 0 1 1
                                             0 0 1 1 0 0 1 1
                                             0 0 1 1 0 0 1 1])))

(buffer-write! v-bass-buf  (take 128 (cycle [1 0 0 1 1 0 1 0
                                             0 1 0 0 1 0 0 1
                                             0 0 1 0 0 1 0 0])))

(buffer-write! v-bass-buf  (take 128 (cycle [1 0 0 0 1 1 0 0
                                             1 0 0 0 1 0 0 0])))

(def m (melody :duration-bus melody-duration-b :offset-bus melody-notes-b
               :beat-count-bus (:count timing/beat-1th) :amp 0
               :out-bus (mix/nkmx :s0)))

(ctl m :amp 1)

(buffer-write! melody-duration-b (take 128 (cycle [1/2 1/4 1/128 1/2])))

(buffer-write! melody-notes-b (take 128 (cycle (map note [:A3 :A4 :B4 :C4]))))
(buffer-write! melody-notes-b (take 128 (cycle (map note [:A3 :A4 :B4 :C4 :D3 :D2 :B2 :D4]))))

(buffer-write! melody-duration-b (take 128 (cycle [1/4 1/8 1/8 1/8 1/4 1/4
                                                   1/8 1/4 1/128 1/4   1/4
                                                   1/8 1/8 1/8 1/4 1/8 1/8])))

(buffer-write! melody-duration-b (take 128 (cycle [1/8 1/8 1/8 1/4 1/8 1/8])))

(buffer-write! melody-notes-b
  (take 128 (cycle (shuffle (map note [:A3 :A5 :B4 :C4 :D3 :D3 :B2 :D4 :D3 :B3 :A3 :B3 :C3 :D3 :E3 :F3 :G4])))))

(buffer-write! melody-notes-b (take 128 (cycle (shuffle (map note data/high-pinging-record)))))
(buffer-write! melody-notes-b (take 128 (cycle (map note data/high-pinging-record))))

(comment
  (kill melody)
  (kill tb)
  (kill o)
  (kill moogey)
  (stop))
(comment
  (fx/fx-distortion-tubescreamer 0)
  (fx/fx-chorus 0)
  (fx/fx-freeverb 0)
  (fx/fx-reverb 0)
  (fx/fx-echo 0)
  (fx/fx-feedback 0))
