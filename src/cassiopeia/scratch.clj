(ns cassiopeia.scratch
  "Here lies demons and a splatter gun of ideas and experiments."
  (:use overtone.live)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.soprano)
  (require [cassiopeia.engine.timing :as time]
           [overtone.studio.fx :as fx]))

(do
  (def singers-g (group "singers"))

  (defonce note-buf (buffer 128))
  (defonce note2-buf (buffer 128))
  (defonce sing-attack (buffer 128))

  (defonce seq-b1 (buffer 128))
  (defonce seq-b2 (buffer 128))
  (defonce seq-b3 (buffer 128))
  (defonce seq-b4 (buffer 128)))


(def singers-ah-p (doseq [i (range 0 3)]
                    (slow-singer
                     [:head singers-g]
                     :note-buf note-buf :amp 2.9
                     :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                     :seq-b seq-b1
                     :beat-num i
                     :index-b ah-index-buffer
                     :num-steps 3)))

(def singers-ah-f (doseq [i (range 0 3)]
                    (slow-singer
                     [:head singers-g]
                     :note-buf note-buf :amp 2.9
                     :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                     :seq-b seq-b3
                     :beat-num i
                     :index-b ah-strong-index-buffer
                     :num-steps 3)))

(def singers-yeh (doseq [i (range 0 3)]
                   (slow-singer
                    [:head singers-g]
                    :note-buf note-buf :amp 2.9
                    :beat-b (:beat time/beat-4th) :count-b (:count time/beat-4th)
                    :seq-b seq-b4
                    :beat-num i
                    :index-b yeh-index-buffer
                    :num-steps 3)))

(pattern! seq-b1
          [1 0 0 0]
          [1 0 0 0]
          [1 0 0 0]
          [0 0 0 0]
          [0 0 0 0])

(pattern! seq-b3
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [1 0 0 0])

(pattern! seq-b4
          [0 0 0 0]
          [0 0 0 0]
          [0 0 0 0]
          [1 0 0 0]
          [1 0 0 0])

;;63 62 64 62 62 62 64
(pattern! note-buf [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])
(pattern! note-buf [70 70 69 69  68 68 70 70])
(pattern! note-buf (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4]))

(on-trigger (:trig-id time/main-beat)
            (fn [x]
              (when (= 0 (mod x 16)))
              (pattern! b     (shuffle [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])))
            ::shuffle-trig)

(remove-event-handler ::shuffle-trig)

(ctl singers-g :attack 0.2)
(ctl singers-g :release 6.)

(pattern! note2-buf [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])
(pattern! note-buf [:Eb4 :D4 :E4 :D4 :D4 :D4 :E4])
(pattern! seq-b2 [1 1 1])

(def fast-singing (fast-singer
                   [:head singers-g]
                   :note-buf note2-buf :amp 0
                   :beat-b (:beat time/beat-1th) :count-b (:count time/beat-1th)))


(kill fast-singer)

(def fast-singing (fast-varied-singer
                   [:head singers-g]
                   :note-buf note2-buf :amp 0
                   :beat-b (:beat time/beat-1th) :count-b (:count time/beat-1th)
                   :attack-b sing-attack))

(ctl singers-g :amp 1)

(pattern! sing-attack
          (repeat 2 [1 1 1/12 1/12])
          (repeat 2 [2 2 2 2]))

(pattern! sing-attack
          (repeat 2 [1/32]))

(pattern! note2-buf
          (repeat 2 [70 68 70 69])
          (repeat 2 [69 70 0 0])
          (repeat 2 [69 68 69 68])
          (repeat 2 [69 69 0 0])
          (repeat 4 [70 68 70 68])
          (repeat 4 [69 68 0 0])
          (repeat 4 [70 69 70 69])
          (repeat 4 [70 70 70 70]))

(pattern! note-buf [70 70 69 69 68 68 70 70])

(pattern! note2-buf [:Eb4 :D4 :F4 :G4 :G4 :G4 :E4 :Eb4 :D4 :E4 :D4 :D4 :D4 :E4])

(n-overtime! fast-singing :amp 0.0 0.2 0.01)
(ctl fast-singing :release 0.1 :attack 0.1)
(ctl fast-singing :release 0.4 :attack 1.0)

(kill fast-singer)
(kill slow-singer)

(comment
  (sing :note 60 :amp 1.49 :pos 0)
  (sing :note 64 :amp 1.09 :pos 0)
  (sing :note 67 :amp 1.09 :pos 0)
  (sing :note 68 :amp 1.09 :pos 0)

  (fx/fx-chorus)
  (fx/fx-reverb)
  (fx/fx-limiter)
  (fx/fx-compressor)
  (fx/fx-rlpf)
  (fx/fx-noise-gate)

  (fx/fx-distortion-tubescreamer)
  (fx/fx-echo))
(stop)




;;(recording-stop)


;;(recording-start "~/Desktop/eta-v8.wav")

(defsynth ding [freq 880 dur 0.2 level 0.25 pan 0.0 out-bus 0]
  (let [amp  (env-gen:ar (env-perc) :action FREE :time-scale dur)
        snd (* (sin-osc:ar freq) amp level)]
    (out out-bus (pan2:ar snd pan))))

(defsynth tick [freq 880 dur 0.1 level 0.25 pan 0.0 out-bus 0]
  (let [amp (env-gen (env-perc) :action FREE :time-scale dur)
        snd (lpf:ar (white-noise:ar) freq)]
    (out out-bus (pan2:ar (* snd amp level) pan))))
-
(tick)
(ding :dur 0.5)

(def note-offset-b (buffer 128))
(def duration-b (buffer 128))

(def note-offsets [:F4 :F4 :F4 :F4 :F4 :F4 :F4
                   :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4 :G4
                   :BB4 :BB4 :BB4 :BB4 :BB4 :BB4
                   :D#4 :D#4 :D#4])

(def duration     [1/7])

(def score (concat
            (map #(+ -5  (note %)) note-offsets)
            (map #(+ -5  (note %)) note-offsets)
            (map #(+ -10 (note %)) note-offsets)
            (map #(+ -5  (note %)) note-offsets)
            (map #(+ -1  (note %)) note-offsets)))

(buffer-write! note-offset-b (take 128 score))

(buffer-write! duration-b (take 128 (cycle duration)))

(ctl timing/root-s :rate 100)

(do
  (defsynth woody-beep [duration-bus 0 beat-count-bus 0 offset-bus 0 amp 1 out-bus 0]
    (let [cnt    (in:kr beat-count-bus)
          offset (buf-rd:kr 1 offset-bus cnt)
          durs   (buf-rd:kr 1 duration-bus cnt)
          trig (t-duty:kr (dseq durs INFINITE))
          freq (demand:kr trig 0 (drand offset INFINITE))
          freq (midicps freq)

          env (env-gen:ar (env-asr :release 0.25 :sustain 0.8) trig)
          tri (* 0.5 (lf-tri:ar freq))
          sin (sin-osc:ar (* 1 freq))
          sin2 (sin-osc:ar (* 1.01 freq))
          wood (bpf:ar (* (white-noise:ar) (line:kr 5 0 0.02)) freq 0.02)
          src (mix [sin sin2 tri wood])
          src (free-verb src)]
      (out:ar out-bus (* amp env (pan2 src)))))

  (kill woody-beep)

  (def w  (woody-beep :duration-bus duration-b :beat-count-bus timing/beat-count-b :offset-bus note-offset-b :amp 4)))

(defsynth deep-saw [freq 100 beat-count-bus 0 offset-bus 0 duration-bus 0 out-bus 0 amp 1 pan 0]
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
        src (free-verb src 0.33 1 1)]
    (out out-bus (* amp [src src]))))

(def bass-duration-b (buffer 32))
(def bass-notes-b    (buffer 32))

(buffer-write! bass-duration-b (take 32 (cycle [(/ 1 3.5)])))
(buffer-write! bass-notes-b (take 32 (cycle (map note [:F2 :F2 :G3 :G2 :G3 :BB2 :BB2 :G2 :G2]))))

(def ps (deep-saw 100 :duration-bus bass-duration-b :beat-count-bus timing/beat-count-b :offset-bus bass-notes-b :amp 0.5))

(ctl ps :freq 200)

(stop)
(kill deep-saw)

(def melody-part2-buf (buffer 128))
(require '[overtone.orchestra.cello :as cello-src])

(cello :length 3 :note (note :A3))
(defsynth cello
  "length options:
   0 -> 0.25
   1 -> 0.5
   2 -> 1
   3 -> 1.5"
  [level 1 rate 1 loop? 0 attack 0 decay 0.5 sustain 1 release 0.1 curve -4 gate 1 beat-count-bus 0
   offset-bus 0 duration-bus 0 beat-trg-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        note   (buf-rd:kr 1 offset-bus cnt)
        length (buf-rd:kr 1 duration-bus cnt)
        trig  (in:kr beat-trg-bus)

        l-buf (index:kr (:id cello-src/length-buffer) length)
        buf (index:kr l-buf note)
        env (env-gen (adsr attack decay sustain release level curve))]
    (out 0 (pan2 (* env (scaled-play-buf 1 buf :trigger trig :loop 1 :start-pos 0))))))


(defonce cello-buf (buffer 128))
(defonce cello-dur (buffer 128))

(cello :beat-count-bus (:count timing/beat-2th) :offset-bus zello-buf :duration-bus zello-dur :beat-trg-bus (:beat timing/beat-2th))

(buffer-write! cello-buf (take 128 (cycle
                                    (map note
                                         [0 0 0 0 0 0 0 0
                                          0 0 0 0 0 0 0 0
                                          0 0 0 0 0 0 0 0
                                          :A3 :E3 :D3 :C4 :D3 :E3 :A3 :C3]))))

(buffer-write! cello-dur (take 128 (cycle [0 0 0 0 0 0 0 0
                                           0 0 0 0 0 0 0 0
                                           0 0 0 0 0 0 0 0
                                           2 2 2 2 2 2 2 1])))

(kill cello)

(require '[overtone.orchestra.oboe :as oboe-src])
(defsynth oboe
  "length options:
   0 -> 0.25
   1 -> 0.5
   2 -> 1
   3 -> 1.5"
  [level 1 rate 1 loop? 0 attack 0 decay 0.5 sustain 1 release 0.1 curve -4 gate 1 beat-count-bus 0 beat-trg-bus 0 offset-bus 0
   duration-bus 0]
  (let [cnt    (in:kr beat-count-bus)
        note   (buf-rd:kr 1 offset-bus cnt)
        length (buf-rd:kr 1 duration-bus cnt)
        trig  (in:kr beat-trg-bus)

        l-buf (index:kr (:id oboe-src/length-buffer) length)
        buf (index:kr l-buf note)
        env (env-gen (adsr attack decay sustain release level curve)
                     :gate gate)]
    (out 0 (pan2 (* env (scaled-play-buf 1 buf :level level :loop 1 :trigger trig :start-pos 0))))))

(oboe :beat-count-bus (:count time/beat-1th) :offset-bus zello-buf :duration-bus zello-dur :beat-trg-bus (:beat time/beat-1th))

(defonce zello-buf (buffer 128))
(defonce zello-dur (buffer 128))

(buffer-write! zello-buf (take 128 (cycle
                                    (map note
                                         [:A3 :C4 :C5 :C4 :G4 :E4 :A4 :C4]))))

(buffer-write! zello-buf (take 128 (cycle
                                    (map note
                                         [:A4 :C4 :C5 :C4 :G4 :E4 :A4 :C4]))))

(buffer-write! zello-dur (take 128 (cycle [3 3 3 3 3 3 3 3 3])))

(ctl timing/root-s :rate 4)

(kill oboe)
(stop)
