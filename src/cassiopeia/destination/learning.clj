(ns cassiopeia.destination.mr
  (:use overtone.live)
  (:use cassiopeia.engine.core)
  (:use cassiopeia.waves.synths)
  (:use cassiopeia.samples)
  (:use cassiopeia.engine.buffers)
  (:use cassiopeia.dirt)
  (:require [cassiopeia.engine.timing :as time]
            [clojure.math.numeric-tower :as math]
            [overtone.studio.fx :as fx]))

(ctl time/root-s :rate 8.)

(defonce note1-dur-b (buffer 256))

(defonce w-note-b (buffer 256))
(defonce w-note2-b (buffer 256))
(defonce w-release-b (buffer 256))
(definst deep-basz2 [amp 1
                    notes-buf 0
                    noise-level 0.05
                    beat-trg-bus (:beat time/beat-4th)
                    beat-bus     (:count time/beat-4th)
                    attack 0.4
                     release 0.9
                     sustain 1
                     decay 1
                    saw-cutoff 300
                    wave 0]
  (let [trg (in:kr beat-trg-bus)
        cnt (in:kr beat-bus)
        note (buf-rd:kr 1 notes-buf cnt)
        ;;        release
        gate-trg (and (> note 0) trg)
        freq (midicps note)
        noize (* noise-level (pink-noise))
        wave (select:ar wave [(saw freq) (pulse freq) (mix [(saw freq) (pulse freq)])])
        src (mix [(lpf wave saw-cutoff)
                  (lpf noize 100)])
        src (g-verb src 200 1 0.2)
        e (env-gen (adsr :attack attack :sustain sustain :decay decay :release release) :gate gate-trg)
        amp (+ (* amp 5) amp)]
        (* amp e src)))



(definst deep-basz [amp 1
                    notes-buf 0
                    noise-level 0.05
                    beat-trg-bus (:beat time/beat-4th)
                    beat-bus     (:count time/beat-4th)
                    attack 0.4
                    release 0.9
                    saw-cutoff 300
                    wave 0]
  (let [trg (in:kr beat-trg-bus)
        cnt (in:kr beat-bus)
        note (buf-rd:kr 1 notes-buf cnt)
;;        release
        gate-trg (and (> note 0) trg)
        freq (midicps note)
        noize (* noise-level (pink-noise))
        wave (select:ar wave [(saw freq) (pulse freq) (mix [(saw freq) (pulse freq)])])
        src (mix [(lpf wave saw-cutoff)
                  (lpf noize 100)])
        src (g-verb src 200 1 0.2)
        e (env-gen (perc attack release) :gate gate-trg)
        amp (+ (* amp 5) amp)
        ]
    (* amp e src)))

(kill deep-basz)

(defonce w-note3-b (buffer 256))

(defonce w-note4-b (buffer 256))
(def per-per (deep-basz :amp 0.0 :noise-level 0.05
                          :notes-buf w-note4-b
                          :beat-trg-bus (:beat time/beat-1th)
                          :beat-bus (:count time/beat-1th)
                          :attack 0.1
                          :release 1.5
                          :saw-cutoff 700
                          :wave 1))

(kill deep-basz)
(defonce w-note5-b (buffer 256))

(do
;;  (kill deep-basz)

  ;;(ctl slow-deep :saw-cutoff 200)
  ;;(ctl highlight-deep :saw-cutoff 500)
  ;;(kill highlight-deep)


  (pattern! w-note4-b [(degrees [1] :major :F4) 0 (degrees [4] :major :F4) (degrees [6] :major :F4) (degrees [4] :major :F4) (degrees [6] :major :F4) (degrees [3] :major :F4) 0 ])

  )

(comment
  (stop)
  (ctl sd-g :release 0.0 :attack 0.0 :beat-trg-bus (:beat time/beat-1th) :beat-bus (:count time/beat-1th) :amp 0)
  (ctl apeg-deep :saw-cutoff 300)

  (ctl time/root-s :rate 0)
  (kill drums-g)

  (map #(ctl %1 :saw-cutoff 600 :amp 0.2) slow-deep-chord-group)
  (map #(ctl %1 :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th)) slow-deep-chord-group)

  (map #(ctl %1 :saw-cutoff 1500 :noise-level 0.5 :amp 0.5 :attack 0.8 :release 20.0 :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th)) slow-deep-chord-group)
  (map #(ctl %1 :saw-cutoff 2000 :noise :amp 0.05) slow-deep-chord-group)

  )
(def slow-deep-chord-group
  (do
    ;;(kill deep-basz2)
    (defonce sd-g (group "slow deep chords"))
    (defonce sd-note1-b (buffer 256))
    (defonce sd-note2-b (buffer 256))
    (defonce sd-note3-b (buffer 256))
    (defonce sd-note4-b (buffer 256))
    [(deep-basz2 [:head sd-g] :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.1 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz2 [:head sd-g]  :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.1 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz2 [:head sd-g] :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.1 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
     (deep-basz2 [:head sd-g]  :saw-cutoff 0 :attack 0.3 :release 6.0 :amp 0.1 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))]))

(def fast-deep-chord-group
  [(deep-basz [:head sd-g] :wave 1 :saw-cutoff 2000 :attack 0.2 :release 0.3 :amp 0.1 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
   (deep-basz [:head sd-g] :wave 1 :saw-cutoff 2000 :attack 0.2 :release 0.3 :amp 0.1 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
   (deep-basz [:head sd-g] :wave 1 :saw-cutoff 2000 :attack 0.2 :release 0.3 :amp 0.1 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))
   (deep-basz [:head sd-g] :wave 1 :saw-cutoff 2000 :attack 0.2 :release 0.3 :amp 0.1 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th))])

(let [_ [0 0 0 0]
      [c21 c22 c23 c24 c25 c26 c27]        (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f21 f22 f23 f24 f25 f26 f27]        (map #(chord-degree %1 :F2 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [fm21 fm22 fm23 fm24 fm25 fm26 fm27] (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f31 f32 f33 f34 f35 f36 f37]        (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f41 f42 f43 f44 f45 f46 f47]        (map #(chord-degree %1 :F4 :major 4) [:i :ii :iii :iv :v :vi :vii])]
  (let [chord-pat
        [
          c21 _
          c23 _
          c24 _
          c23 c25

          c21 _
          c23 _
          c24 _
          c25 c27
          ]]
    (let [chord-bufs (shuffle [sd-note1-b sd-note2-b sd-note3-b sd-note4-b])] ;; Play around with some random inversions
      (dotimes [chord-idx (count chord-bufs)]
        (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))


(do
  (doseq [chord-g slow-deep-chord-group] (ctl chord-g :saw-cutoff 3000 :amp 0.7 :attack 0.6 :noise-level 0 :sustain 10.0 :decay 10.0 :release 10.0 :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-8th)))
  (ctl per-per :amp 0.4))

(do (defonce drums-g (group "drums")) (defonce drum-effects-g (group "drums effects for extra sweetness")) (defbufs 128 [bass-notes-buf bass-notes2-buf hats-buf kick-seq-buf white-seq-buf effects-seq-buf effects2-seq-buf bass-notes-buf]))

(do
  (def kicker (doseq [i (range 0 96)] (kick2 [:head drums-g] :note-buf bass-notes-buf :seq-buf  kick-seq-buf :num-steps 96 :beat-num i :noise 0.05 :amp 4.2 :mod-index 0.4 :mod-freq 21.0 :mode-freq 20)))

  (def white (doall (map
                     #(whitenoise-hat
                       [:head drums-g]
                       :amp 5.5
                       :seq-buf  white-seq-buf
                       :beat-bus     (:count time/beat-1th)
                       :beat-trg-bus (:beat time/beat-1th)
                       :num-steps 24
                       :beat-num %1) (range 0 24))))

  (pattern! white-seq-buf [1])

  (pattern! white-seq-buf [0 0 0 0 1 0 0 0   0 0 1 0 0 0 0 0])
  (pattern! kick-seq-buf  [1 0 0 1 0 0 0 0   1 0 0 0 0 0 0 0])

  (pattern! bass-notes-buf (degrees [3] :major :F1)))


;;BEGIN
;;3 voices

;;Drums enter + melody
;;High voice enters
;;Drums stop, rising tension
;;Drums kick back in, background note pushing
;;Drumsdrop
;;END
(stop)

(kill slow-deep-chord-group)
(kill fast-deep-chord-group)

(let [_ [0 0 0 0]
      [c21 c22 c23 c24 c25 c26 c27]        (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f21 f22 f23 f24 f25 f26 f27]        (map #(chord-degree %1 :F2 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [fm21 fm22 fm23 fm24 fm25 fm26 fm27] (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f31 f32 f33 f34 f35 f36 f37]        (map #(chord-degree %1 :F3 :major 4) [:i :ii :iii :iv :v :vi :vii])
      [f41 f42 f43 f44 f45 f46 f47]        (map #(chord-degree %1 :F4 :major 4) [:i :ii :iii :iv :v :vi :vii])]
  (let [chord-pat
        [
         c23 c23 c23 c23  c21 c23 c23 c24 c21
         c24 c24 c24 c24  c25 c27 c27 c24 c21
         ]]
    (let [chord-bufs (shuffle [sd-note1-b sd-note2-b sd-note3-b sd-note4-b])] ;; Play around with some random inversions
      (dotimes [chord-idx (count chord-bufs)]
                (pattern! (nth chord-bufs chord-idx) (map #(if (> (count %1) chord-idx) (nth %1 chord-idx) 0) chord-pat))))))

(def melody [(deep-basz [:head sd-g] :wave 0 :saw-cutoff 800 :attack 1.9 :release 0.6 :amp 0.2 :noise-level 0.05 :notes-buf sd-note1-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
             (deep-basz [:head sd-g] :wave 1 :saw-cutoff 800 :attack 1.9 :release 0.6 :amp 0.2 :noise-level 0.05 :notes-buf sd-note2-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
             (deep-basz [:head sd-g] :wave 0 :saw-cutoff 800 :attack 1.9 :release 0.6 :amp 0.2 :noise-level 0.05 :notes-buf sd-note3-b :beat-trg-bus (:beat time/beat-4th) :beat-bus (:count time/beat-4th))
             (deep-basz [:head sd-g] :wave 1 :saw-cutoff 800 :attack 1.9 :release 0.6 :amp 0.2 :noise-level 0.05 :notes-buf sd-note4-b :beat-trg-bus (:beat time/beat-8th) :beat-bus (:count time/beat-4th))])

(do
  (map #(ctl %1 :amp 0) (flatten [melody]))
  (ctl drums-g :amp 0)

  (ctl per-per :beat-bus (:count time/beat-2th) :beat-trg-bus (:count time/beat-2th) :release 10 :attack 0.01)

  (pattern! w-note4-b (degrees [1] :major :F4 ) 0
            (degrees [3] :major :F4) 0
            (degrees [4] :major :F4) 0
            (degrees [3] :major :F4) (degrees [5] :major :F4)

            (degrees [1] :major :F4) 0
            (degrees [3] :major :F4) 0
            (degrees [4] :major :F4) 0
            (degrees [5] :major :F4) (degrees [7] :major :F4)
            ))

(stop)
