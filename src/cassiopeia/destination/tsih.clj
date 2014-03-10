(ns cassiopeia.destination.tsih
  (:require [cassiopeia.engine.timing :as time]
            [overtone.studio.fx :as fx]
            [cassiopeia.engine.mixers :as mix]
            [overtone.inst.synth :as s]
            ;;            [overtone.inst.drum :as drum]
            )
  (:use overtone.live))

(defn buffer-cycle! [buf list]
   (buffer-write! buf (take (buffer-size buf) (cycle list))))

(defonce power-kick-g (group))
(defonce power-kick-seq (buffer 16))
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

(comment
  (do
    (kill quick-kick)
    (def power-kick (doall (map-indexed
                            #(quick-kick
                              [:head power-kick-g]
                              :amp 0.5
                              :note-buf bass-notes-buf
                              :seq-buf power-kick-seq
                              :beat-bus (:count time/beat-1th)
                              :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :beat-num %2) (range 0 16)))))
  )

(defonce melody-duration-b (buffer 128))
(defonce melody-notes-b    (buffer 128))
(defonce melody-pan-buf (buffer 128))
(defsynth melody [duration-bus 0 room 0.5 damp 0.5 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0 pitch-dis 0 time-dis 0 cut 2000 pan-level 0]
  (let [cnt    (in:kr beat-count-bus)
        offset (buf-rd:kr 1 offset-bus cnt)
        durs   (buf-rd:kr 1 duration-bus cnt)

        trig (and (not= durs 0) (t-duty:kr (dseq durs INFINITE)))
        freq (demand:kr trig 0 (drand offset INFINITE))
        freq (midicps freq)

        p1 (pulse freq (* 0.1 (/ (+ 1.2 (sin-osc:kr 1)))))
        p2 (pulse freq (* 0.8 (/ (+ 1.2 (sin-osc:kr 1) 0.7) 2)))
        p3 (pulse (* 2 freq)  (mul-add:kr (lf-noise1:kr 10) 1 4))
        tri (* 0.3 (lf-tri:ar freq))

        env (env-gen:ar (env-asr :release 1 :sustain 1 :attack 0.01) trig)
        src (rlpf (mix [p1 p2 p3
                        tri
                        (saw (* 0.6 freq))
                        (saw (* 0.4 freq))
                        (saw (* 0.8 freq))]))]
    (out out-bus (pan2 (* amp env src) pan-level))))

(comment
  (do
    (kill melody)
    (def m (melody :duration-bus melody-duration-b :offset-bus melody-notes-b
                   :beat-count-bus (:count time/beat-1th) :amp 10)))
  (stop))

(defonce kick-buf (buffer 16))
(defsynth kick2 [freq      {:default 80 :min 10 :max 20000 :step 1}
                amp       {:default 0.8 :min 0.001 :max 1.0 :step 0.001}
                mod-freq  {:default 5 :min 0.001 :max 10.0 :step 0.01}
                mod-index {:default 5 :min 0.001 :max 10.0 :step 0.01}
                sustain   {:default 0.4 :min 0.001 :max 1.0 :step 0.001}
                noise     {:default 0.025 :min 0.001 :max 1.0 :step 0.001}
                beat-bus 0
                beat-trg-bus 0
                note-buf 0
                seq-buf 0
                beat-num 0
                 num-steps 8
                 out-bus 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)

        pitch-contour (line:kr (* 2 freq) freq 0.02)
        drum (lpf (sin-osc pitch-contour (sin-osc mod-freq (/ mod-index 1.3))) 1000)
        drum-env (env-gen (perc 0.005 sustain) :gate bar-trg)
        hit (hpf (* noise (white-noise)) 500)
        hit (lpf hit (line 6000 500 0.03))
        hit-env (env-gen (perc))
        src (* amp (+ (* drum drum-env) (* hit hit-env)))]
    (out out-bus (pan2 src))))

(defsynth pulsar [note-buf 0 beat-bus 0 beat-trg-bus 0]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trg (and (not= note 0) trg)

        freq (midicps note)
        src (lpf (saw freq) 400)
        e (env-gen (perc) :gate gate-trg)]
    (out 0 (pan2:ar (* e  src) (sin-osc:kr 2)))))

(defonce notes-buf (buffer 128))
(defonce shrill-buf (buffer 128))

(defsynth shrill-pulsar [note-buf 0 beat-bus 0 beat-trg-bus 0 size 1 r 0]
  (let [cnt (in:kr beat-bus)
        note (buf-rd:kr 1 note-buf cnt)
        trg (in:kr beat-trg-bus)
        gate-trig (and (not= 0 note) trg)
        freq (midicps note)
        src (+ [(lpf (saw freq) 500)] [(sin-osc (* 1.01 freq))] )
;;        src (free-verb src size r)
        e (env-gen (adsr :release 1) :gate gate-trig)]
    (out 0 (pan2:ar (* e  src)))))

(comment
  (do
    (kill shrill-pulsar)

    (buffer-cycle! shrill-buf (map note [:C4]))

    (def q (shrill-pulsar :beat-trg-bus (:beat time/beat-1th)
                          :beat-bus (:count time/beat-1th)
                          :note-buf shrill-buf))

    )
  )

(defonce growl-buf (buffer 128))
(defonce growl-amp-buf (buffer 128))

(do
  (defsynth growl [note-buf 0 beat-bus 0 beat-trg-bus 0 amp 1]
    (let [cnt (in:kr beat-bus)
          note (buf-rd:kr 1 note-buf cnt)
          trg (in:kr beat-trg-bus)
          famp (buf-rd:kr 1 growl-amp-buf cnt)
          gate-trig (and (not= 0 note) trg)

          freq (midicps note)
          e (env-gen (perc :attack 10 :sustain 2 :release 2) :gate gate-trig)
          src (lpf (mix [(saw (* 0.25 freq))
                         (sin-osc (* 1.01 freq))]))
          src (pitch-shift src 0.4 1 0 0.01)
          ]
      (out 0 (pan2:ar (* famp  amp e src)))))

  (kill growl)


  (def g (growl
          :amp 2
          :beat-trg-bus (:beat time/beat-8th)
          :beat-bus (:count time/beat-8th)
          :note-buf growl-buf))
  )

(defonce high-kick-g (group "high kicks"))
(defsynth glass-ping
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

(defsynth dark-ambience [out-bus 0 amp 1 mul 0.2 room-size 70 rev-time 99 ring-freq 60 ring-mul 55]
  (let [pink (hpf:ar (* (* 0.005 (pink-noise)) (line:kr 0 1 9)) 5)
        src1 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 0)) mul)
        src2 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 1)) mul)
        src3 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 2)) mul)
        src (tanh (g-verb (sum [src1 src2 src3]) room-size rev-time))]
    (out out-bus (* amp src))))

(defonce bass-notes-buf (buffer 32))
(defonce ping-bass-seq-buf (buffer 32))

(defonce phase-bass-buf (buffer 32))
(defonce bazz-g (group "bazz group"))
(defsynth bazz [out-bus 0 beat-bus 0 beat-trg-bus 0 note-buf 0 seq-buf 0 beat-num 0 num-steps 0
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

(defonce white-seq-buf (buffer 32))
(defonce white-notes-buf (buffer 32))
(defonce white-g (group "whitenoise-hat"))
(defsynth whitenoise-hat [out-bus 0 seq-buf 0 beat-bus 0 beat-trg-bus 0 num-steps 0 beat-num 0 amp 1]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)
        w (* 0.01 (white-noise:ar))
        e (env-gen (perc :attack 0 :release 1) :gate bar-trg)]
    (out out-bus (pan2 (* amp e w)))))

;;;;;;;;;;;
;; Score ;;
;;;;;;;;;;;

(def high-pings (doall (map-indexed
                      #(glass-ping
                        [:head high-kick-g]
                        :amp 1
                        :note-buf bass-notes-buf
                        :seq-buf  ping-bass-seq-buf
                        :beat-bus (:count time/beat-1th)
                        :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

(ctl high-kick-g :beat-bus (:count time/beat-2th) :beat-trg-bus (:beat time/beat-2th))
(ctl high-kick-g :beat-bus (:count time/beat-1th) :beat-trg-bus (:beat time/beat-1th))

(def high-pings-echo (doall (map-indexed
                        #(glass-ping
                          [:head high-kick-g]
                          :amp 1
                          :note-buf bass-notes-buf
                          :seq-buf ping-bass-seq-buf
                          :beat-bus (:count time/beat-1th)
                          :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num (+ 4 %2)) (range 0 18))))

(def high-pings-echo (doall (map-indexed
                             #(glass-ping
                               [:head high-kick-g]
                               :amp 1
                               :note-buf bass-notes-buf
                               :seq-buf ping-bass-seq-buf
                               :beat-bus (:count time/beat-1th)
                               :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

(defonce mid-ping-notes-buf (buffer 128))
(defonce mid-ping-seq-buf  (buffer 128))
(def mid-pings (doall (map-indexed
                       #(glass-ping
                         [:head high-kick-g]
                         :amp 1
                         :note-buf mid-ping-notes-buf
                         :seq-buf mid-ping-seq-buf
                         :freq  (mod %1 8)
                         :beat-bus (:count time/beat-1th)
                         :beat-trg-bus (:beat time/beat-1th) :num-steps 18 :beat-num %2) (range 0 18))))

(kill mid-pings)
(kill glass-ping)
(kill bazz)

(doseq [i (range 0 32)]
  (whitenoise-hat
   [:head white-g]
   :note-buf white-notes-buf
   :amp (+ 0.1 (/  i 16))
   :seq-buf  white-seq-buf
   :beat-bus     (:count time/beat-1th)
   :beat-trg-bus (:beat time/beat-1th)
   :num-steps 32
   :beat-num i))

(def kick2-g (group "kick2"))

(defonce kick-seq-buf (buffer 32))

(do
  (doseq [i (range 0 32)]
    (bazz
     [:head bazz-g]
     :amp 0.7
     :mix (nth (take 32 (cycle [0.1 0.05])) i)
     :room 2
     ;;   :damp 0.6
     :note-buf bass-notes-buf
     :seq-buf phase-bass-buf
     :beat-bus     (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th) :num-steps 32 :beat-num i))
  (ctl bazz-g :damp 0.6)

  (def power-kick (doall (map-indexed
                          #(quick-kick
                            [:head power-kick-g]
                            :amp 0.5
                            :note-buf bass-notes-buf
                            :seq-buf power-kick-seq
                            :beat-bus (:count time/beat-1th)
                            :beat-trg-bus (:beat time/beat-1th) :num-steps 16 :beat-num %2) (range 0 16))))
  (buffer-cycle! power-kick-seq [1 0 0 0 0 0 0 0])

  (doseq [i (range 0 32)]
    (kick2
     [:head kick2-g]
     :note-buf bass-notes-buf
     :seq-buf  kick-seq-buf
     :beat-bus     (:count time/beat-1th)
     :beat-trg-bus (:beat time/beat-1th)
     :num-steps 32
     :beat-num i))

  (ctl time/root-s :rate 4)
  (buffer-cycle! kick-seq-buf [1 0 0 0])
  (buffer-cycle! white-seq-buf [0]))

(kill power-kick)
(kill kick2)
(ctl time/root-s :rate 0)

(do (buffer-cycle! white-seq-buf [1 0 0 0 1 0 0 0 1 0 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]))

(kill whitenoise-hat)
(kill kick2-g)
(kill bazz)

(buffer-cycle! kick-seq-buf [0 0 1 0 0 1 0 0 1 1])

;;(ctl bazz-g :amp 0)

(buffer-cycle! bass-notes-buf (map note [:A1 :A1 :A1 :A1 :A1 :A1]))
(buffer-cycle! bass-notes-buf (map note [:A5 :A6 :A2 :A2 :A6 :A5]))
(buffer-cycle! bass-notes-buf (map note [:D5 :D6 :D2 :D2 :D6 :D5]))
(buffer-cycle! bass-notes-buf (map note [:E5 :E6 :E2 :E2 :E6 :E5]))

(buffer-write! bass-notes-buf (take 32 (cycle (map note [:A4 :E4 :E2 :E2 :D6 :D5
                                                         :E4 :E4 :A2 :A2 :D6 :D5
                                                         :D4 :D4 :A2 :E2 :D6 :D5]))))

(buffer-cycle! mid-ping-seq-buf [0 1])
(buffer-cycle! mid-ping-notes-buf (map note [:A4 :A4 :D4 :D4 :D4 :E4]))
(buffer-cycle! mid-ping-notes-buf (map note [:A4 :A4 :D4 :D4 :D4 :E4
                                             0 0 0 0 0 0
                                             :A4 :A4 :D4 :D4 :D4 :F#4
                                             0 0 0 0 0 0
                                             :A4 :A4 :D4 :D4 :D4 :F#4
                                             0 0 0 0 0 0]))

(buffer-write! phase-bass-buf  (take 32 (cycle [0 1])))
(buffer-write! phase-bass-buf  (take 32 (cycle [1 1 0 0 0 0 0 0])))

(buffer-cycle!  kick-seq-buf [1 0 0 0])

(ctl time/root-s :rate 4)

(def p (pulsar :beat-trg-bus (:beat time/beat-1th)
               :beat-bus (:count time/beat-1th)
               :note-buf notes-buf))

(def q (shrill-pulsar :beat-trg-bus (:beat time/beat-1th)
                      :beat-bus (:count time/beat-1th)
                      :note-buf shrill-buf))

(def g (growl :amp 1.2
              :beat-trg-bus (:beat time/beat-4th)
              :beat-bus (:count time/beat-4th)
              :note-buf growl-buf))

(buffer-cycle! growl-amp-buf       [1   1   0  1   1  0  1   1  0  1   1  0  1   1])
(buffer-cycle! growl-buf (map note [:D3 :D3 0 :E3 :E3 0 :A4 :A4 0 :D4 :D4 0 :F#4 :F#4]))


(buffer-cycle! growl-amp-buf       [1   1   0  1   1])
(buffer-cycle! growl-buf (map note [:D3 :D3 0 :E3 :E3]))
(buffer-cycle! growl-buf (map note [:D3 :D3 0 :D3 :D3]))

(ctl g :amp 1.5)

(s/rise-fall-pad :freq (midi->hz (note :A3)))

(def dark (dark-ambience :mul 0.4
                         :amp 0.5
                         :ring-freq (midi->hz (note :A3))))

(ctl dark :ring-freq (midi->hz (note :A3)))

(buffer-cycle! notes-buf (map note  [:F#3 0 :F#3]))
(buffer-cycle! notes-buf (map note  [:D3 0 :D3]))
(buffer-cycle! shrill-buf (map note [0 :E3 0 :E3 0]))

(buffer-cycle! notes-buf (cycle [0]))
(buffer-cycle! shrill-buf (cycle [0]))

(buffer-cycle! bass-notes-buf (map note [:A2]))
(buffer-cycle! ping-bass-seq-buf [1 0 1 0])

(kill shrill-pulsar)
(kill pulsar)
(kill growl)

(def m (melody :duration-bus melody-duration-b :offset-bus melody-notes-b
               :beat-count-bus (:count time/beat-2th) :amp 2))

(kill melody)

(buffer-write! melody-notes-b
               (take 128 (cycle (shuffle (map note [:A3 :A4 :A4 :A5 :A5 :A6
                                                    :A3 :A4 :A4 :A5 :A5 :A6
                                                    :A3 :A4 :A4 :A5 :A5 :A6
                                                    :D3 :D4 :D4 :D5 :D5 :D6
                                                    :E3 :E4 :E4 :E5 :E5 :E6
                                                    :E3 :E4 :E4 :E5 :E5 :E6
                                                    ])))))

(buffer-write! melody-notes-b
               (take 128 (cycle (shuffle (map note [:A4 :A4 :D4 :D4 :C4 :C4])))))

(buffer-write! melody-duration-b (take 128 (cycle [1/4 1/2 1/2 1/4 1/4 1/4])))

(buffer-write! melody-notes-b
               (take 128 (cycle (shuffle (map note [:E3 :E4 :E4 :E4])))))

(buffer-cycle! melody-pan-buf  [0.9 0.8 0.8 -0.9 -0.8 -0.8])
;;(buffer-cycle! melody-duration-b [1/2 1/8 1/8 0])

(buffer-write! melody-notes-b
               (take 128 (cycle (shuffle (map note [:A3 :E4 :E4 :E5 :E5 :E6])))))

(buffer-write! melody-notes-b
               (take 128 (cycle (shuffle (map note [:C2 :D2 :E2 :E2 :E2 :E4 ])))))

(buffer-write! melody-duration-b (take 128 (cycle [1/8 1/4 1/4 1/8 1/8 1/8])))

(comment
  (def fx2 (fx/fx-chorus 0))
  (def fx3 (fx/fx-echo 0))

  (kill fx2)
  (kill melody)
  (kill whitenoise-hat)
  (kill kick2-g)
  (kill bazz)
  (kill power-kick)
  (kill kick2)
  (kill mid-pings)
  (kill glass-ping)
  (kill bazz)

  (stop)
)
