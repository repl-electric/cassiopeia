(ns cassiopeia.destination.ruchbah
  "
 ######
 #     # #    #  ####  #    # #####    ##   #    #
 #     # #    # #    # #    # #    #  #  #  #    #
 ######  #    # #      ###### #####  #    # ######
 #   #   #    # #      #    # #    # ###### #    #
 #    #  #    # #    # #    # #    # #    # #    #
 #     #  ####   ####  #    # #####  #    # #    #

 An Algol-type eclipsing variable star. It appears to have a blue-white hue and it is 99 light-years from Earth.
"
  (:use overtone.live)
  (:require [cassiopeia.engine.timing :as timing]
            [overtone.inst.synth :as s]))

(defsynth deep-saw [freq 100 beat-count-bus 0 offset-bus 0 duration-bus 0 out-bus 0 amp 1 pan 0 room 0.5 damp 0.5]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        saw1 (lf-saw:ar (* 0.5 freq))
        saw2 (lf-saw:ar (* 0.25 freq))
        sin1 (sin-osc freq)
        sin2 (sin-osc (* 1.01 freq))
        src (mix [saw1 saw2 sin1 sin2])
        env (env-gen:ar (env-asr) trig)
        src (lpf:ar src)
        src (free-verb :in src :mix 0.33 :room room :damp damp)]
    (out out-bus (pan2 (* amp [src src]) pan))))

(defsynth melody [duration-bus 0 room 0.5 damp 0.5 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)
        trig (t-duty:kr (dseq durs INFINITE))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        env (env-gen:ar (env-asr :release 0.25 :sustain 0.8) trig)
        src (* 0.5 (lf-tri:ar freq))]
    (out:ar out-bus (* amp env (pan2 src (t-rand:kr -1 1 trig))))))

(def flow-buf (buffer 128))

(definst overpad
  [amp 0.7 attack 0.001 release 2 note-buf 0 beat-count-bus 0 beat-trg-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        note (buf-rd:kr 1 note-buf cnt)

        trg (mod cnt 2)

        freq  (midicps note)
        env   (env-gen (perc attack release) trg)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
        audio))

(definst tb303
  [note-buf 0
   beat-count-bus 0
   wave       {:default 1 :min 0 :max 2 :step 1}
   r          {:default 0.8 :min 0.01 :max 0.99 :step 0.01}
   attack     {:default 0.01 :min 0.001 :max 4 :step 0.001}
   decay      {:default 0.1 :min 0.001 :max 4 :step 0.001}
   sustain    {:default 0.6 :min 0.001 :max 0.99 :step 0.001}
   release    {:default 0.01 :min 0.001 :max 4 :step 0.001}
   cutoff     {:default 100 :min 1 :max 20000 :step 1}
   env-amount {:default 0.01 :min 0.001 :max 4 :step 0.001}
   amp        {:default 0.5 :min 0 :max 1 :step 0.01}]
  (let [cnt (in:kr beat-count-bus)
        note (buf-rd:kr 1 note-buf cnt)

        freq       (midicps note)
        freqs      [freq (* 1.01 freq)]
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :action FREE)
        fil-env    (env-gen (perc))
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves      (* vol-env
                      [(saw freqs)
                       (pulse freqs 0.5)
                       (lf-tri freqs)])
        selector   (select wave waves)
        filt       (rlpf selector fil-cutoff r)]
    (* amp filt)))


(tb303 :note (note :A3) :attack 3 :amp 1.5 :sustain 0.9 :release 4 :amp 0.9
       :note-buf flow-buf
       :beat-count-bus (:beat timing/beat-4th))

(def moo (s/mooger (note :A3)))

(ctl moo :note (note :A2))

(s/cs80lead :freq 200)
(stop)


(def o (overpad :release 16 :note-buf flow-buf :beat-count-bus (:count timing/beat-4th)
                :beat-trig-bus (:beat timing/beat-4th) :amp 0.2 :attack 5))

(ctl o :amp 0.3)

(kill overpad)

(buffer-write! flow-buf (take 128 (cycle (map note [:A4 :C4]))))

(defonce bass-duration-b (buffer 128))
(defonce bass-notes-b    (buffer 128))

(def deep (deep-saw :duration-bus bass-duration-b :offset-bus bass-notes-b
                    :beat-count-bus (:count timing/beat-2th) :amp 0))

;;(def dum-samples-set [beatbox-kick-s tom-s shake-s shake2-s shake1-s shake2d-s clap-s clap2-s])

(buffer-write! bass-duration-b (take 128 (cycle           [1 1 1 1])))
(buffer-write! bass-notes-b    (take 128 (cycle (map note [:A3 :E3 :D3 :C4  :A3 :E3 :D3 :C3]))))

(buffer-write! bass-duration-b (take 128 (cycle           [1/2])))
(buffer-write! bass-notes-b    (take 128 (cycle (map note [:D3 :E3 :D3 :C3
                                                           :D3 :E3 :D3 :C3]))))

(defonce melody-duration-b (buffer 128))
(defonce melody-notes-b    (buffer 128))

(def m (melody :duration-bus melody-duration-b :offset-bus melody-notes-b
               :beat-count-bus (:count timing/beat-1th) :amp 0))

(ctl m :amp 0.3)

(buffer-write! melody-duration-b (take 128 (cycle [1/2 1/4 1/4 1/2])))
(buffer-write! melody-notes-b (take 128 (cycle (map note [:A3 :A4 :B4 :C4]))))
(buffer-write! melody-duration-b (take 128 (cycle [1/4 1/4 1/4 1/4])))

(kill s/mooger)
(s/cs80lead :freq 90 :amp 0.2)
(kill s/cs80lead)

(s/overpad :note 60 :release 16)

(s/rise-fall-pad :freq (midi->hz (note :A3)))
(ctl deep :amp 0.5)

(kill s/rise-fall-pad)

(def bass-notes-buf (buffer 8))
(def phase-bass-buf (buffer 8))

(definst vintage-bass
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

(doseq [i (range 0 9)]
  (vintage-bass :amp 0.6 :note-buf bass-notes-buf
                :seq-buf phase-bass-buf
                :beat-bus (:count timing/beat-1th)
                  :beat-trg-bus (:beat timing/beat-1th) :num-steps 8 :beat-num i))

(buffer-write! bass-notes-buf  (take 8 (cycle (map note [:E2]))))
(buffer-write! bass-notes-buf  (take 8 (cycle (map note [:D3]))))
(buffer-write! phase-bass-buf  [1 0 0 0 0 0 0 0])

(stop)
