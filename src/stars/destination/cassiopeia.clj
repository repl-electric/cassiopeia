(ns stars.cassiopeia
  (:use [overtone.live]
        [stars.warm-up]
        [stars.samples]
        [stars.engine.mixer]
        [overtone.synth.sampled-piano])
  (:require [stars.engine.timing :as tim]
            [stars.synths.synths :as syn]))

(def satellite-data
  "Noise from a satellite pointed towards the Cassiopeia constellation"
  [9 11 5 11 5 11 14 11 14 11 14 11 14 17 14 17 14 9 4 14 9 17
   14 17 9 21 9 4 14 4 9 4 9 21 4 9 21 4 9 14 17 14 17 14 17 9 17 14 17 0 17 0 17 0 14
   17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 4 0 4 0 4 0 4 0 4 0 17 9 17 14 17 4 14 9 4 14
   17 14 9 14 17 14 21 4 17 0 17 14 9 14 17 0 4 0 4 0 4 0 4 0 4 0 4 17 14 17 0 17 0 14 9
   14 4 17 0 4 9 4 9 14 9 14 9 4 9 4 9 4 14 0 17 0 14 17 14 17 0 14 0 14 17 14 17 14 17 14
   17 14 17 14 17 0 17 4 17 14 17 0 17 14 4 0 14 17 14 17 14 17 14 17 0 17 0 4 0 4 0 4 9
   4 9 4 9 14 19 14 19 0 5 0 5 9 14 19 2 11 19 2 7 16 21 7 11 16 4 16 21 4 9 21 9 0 9 14 2
   5 11 14 2 11 19 7 21 7 4 12 21 4 17 0 4 14 5 2 19 2 11 7 11 21 7 12 17 9 14 9 11 5 2 19
   11 7 21 7 16 12 9 17 0 19 9 14 2 11 19 7 16 11 7 16 4 16 12 21 9 16 21 4 17 4 0 19 0 17
   9 4 0 9 14 19 14 19 9 14 9 0 17 14 9 21 17 12 9 21 16 12 7 21 16 11 19 11 2 11 14 2 5 19
   9 4 14 9 16 21 2 7 21 7 11 16 9 14 12 9 4 21 4 21 19 2 16 19 5 14 19 9 14 0 9 21 4
   12 7 21 2 21 7 2 11 2 14 5 19 9 0 17 9 4 21 17 9 21 12 7 4 21 11 7 2 7 21 16 11 7 2 19 14
   5 2 19 14 9 5 19 14 9 4 0 17 9 17 0 14 17 14 9 14 9 4 9 14 17 9 4 9 14 9 14 9 14 9 14 17
   14 9 4 14 9 14 9 14 17 14 9 17 14 4 9 4 9 14 17 0 17 0 9 14 17 0 17 0 17 9 14 17 14 17
   9 14 9 14 17 14 17 14 17 14 17 14 17 14 17 14 9 14 9 14 9 14 17 14 17 14 17 14 17
   14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 14 17 0 17 0 17 0 17 0 17 0 17 0 17 0 17
   0 17 0 17 0 17 0 17 0 17 0 17 19 14 19 11 14 11 14 11 14])

(defsynth spacey [out-bus 0 amp 1]
  (out out-bus (* amp (g-verb (blip (mouse-y 24 48) (mouse-x 1 100)) 200 8))))

(defsynth space-organ-opera [out-bus 0 amp 1 room 200]
  (let [f     (map #(midicps (duty:kr % 0 (dseq satellite-data INF)))
                   [1 1/2 1/4])
        tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/2) 3 4))
                   f
                   [1 4 8 16 32 64])]
    (out out-bus (* amp (g-verb (sum tones) room 8)))))

(defonce space-organ-g (group))

(defsynth space-organ [out-bus 0 tone 1 duration 3 amp 1]
  (let [f     (map #(midicps (duty:kr % 0 (dseq 2 4)))
                   [1])
        tones (map #(blip (* % %2) (mul-add:kr (lf-noise1:kr 1/8) 2 4))
                   f
                   [tone])]
    (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE)))))

(defsynth plain-space-organ [out-bus 0 tone 1 duration 3 amp 1]
  (let [tones (map #(blip (* % 2) (mul-add:kr 1/8 1 4)) [tone])]
    (out out-bus (* amp (g-verb (sum tones) 200 8) (line 1 0 duration FREE)))))

(def windy (sample (freesound-path 17553)) :loop? true)
(ctl windy :rate 1 :vol 1 :out-bus (nkmx :m0))

;;Score

(spacey)

(space-organ :tone 50 :out-bus (nkmx :s1) :amp 0.05)

(doseq [[tone len] [[32 2] [36 4] [32 4] [32 4]]]
  (space-organ :tone tone :out-bus (nkmx :s1) :amp 0.35) (Thread/sleep (* 1000 len)))
(doseq [[tone len] [[24 5] [32 2] [18 4] [19 3]]]
  (space-organ :tone tone :out-bus 0 :amp 0.35) (Thread/sleep (* 500 len)))

(doseq [tone [16 32]]
  (space-organ :tone tone :out-bus (nkmx :s1) :amp 0.35 :duration 5) (Thread/sleep 2000))

(periodic 2000 #(space-organ :amp 0.35 :tone 4 :out-bus (nkmx :s2)))

(periodic 1000 #(doseq [[tone dur] [[4 1] [ 8 1] [16 1] [32 1] [16 1] [8 1]]]
                  (space-organ :tone tone :out-bus (nkmx :s2) :amp 0.35 :duration dur) (Thread/sleep 500)))


(kill beat)
(stop)

(volume 1)

(spacey :out-bus (nkmx :s2))
(def opera (space-organ-opera [:head space-organ-g] :amp 0.2 :out-bus (nkmx :s1)))

(ctl opera :amp 0.2)

(def soundscape (sample-player (sample (freesound-path 38969)) :loop? true :rate 0.5))

(ctl soundscape :rate 0.15 :amp 0.1 :out-bus (nkmx :s1))
(kill soundscape)

(def waves ((:ocean-waves2 atmossy) :loop? true :rate 0.50 :amp 0.1 :out-bus (nkmx :s2)))
(ctl waves :amp 1 :rate 1)

(kill waves)
(stop)

(defonce beat-rep-key (uuid))
(defonce get-beat-s (get-beat))

(metronome/start lp :mixer count-trig-id beat-rep-key)

(def pad-samples [kick-s snare-s high-hat-open-s heavy-bass-kick-s clap-s sizzling-high-hat-s])
(def lp-sequencer (mk-sequencer "launchpad-sequencer" pad-samples phrase-size beat-cnt-bus beat-trg-bus 0))
(defonce refresh-beat-key (uuid))

  ;; 1 0 0 0 1 0 0 0 - 1 0 0 0 1 0 0 0
  ;; 0 0 1 0 0 0 1 0 - 0 0 1 0 0 0 0 0
  ;; 0 1 0 0 0 0 0 0 - 0 1 0 0 0 0 0 0
  ;; 0 0 0 0 0 0 0 0 - 0 0 0 0 0 0 1 0
  ;; 0 1 0 0 0 0 0 0 - 0 0 0 0 1 0 0 0


(on-trigger count-trig-id (beat/grid-refresh lp lp-sequencer phrase-size) refresh-beat-key)
(beat/setup-side-controls :up lp-sequencer)

;;Adjust bpm
(bind :up :7x6 (fn [] (ctl b-trg :div (swap! current-beat inc))))
(bind :up :7x5 (fn [] (ctl b-trg :div (swap! current-beat dec))))

;;Shutdown
(bind :up :arm  (fn [lp] (beat/off lp lp-sequencer)))


  (def space-notes [8 16 32 16 8])
  (def space-tones [8 16 24])
  (defsynth space-organ [out-bus 0 vol 1 size 200 r 8 noise 10 trig]
    (let [notes (map #(midicps (duty:kr % 0 (dseq space-notes INF))) [1 1/2 1/4 1/8])
          tones (map (fn [note tone] (blip (* note tone)
                                          (mul-add:kr (lf-noise1:kr noise) 3 4))) notes space-tones)]
      (out out-bus (* vol (g-verb (sum tones) size r)))))

  (def so (space-organ :vol 0.5))
  (ctl so :vol 0.01)

  (ctl so :r 200)
  (ctl so :size 0)
  (ctl so :size 900)
  (ctl so :noise 10)

  (ctl so :vol 0.5)

  (ctl so)

  (stop)
  (demo 5 (duty 1 0 (dseq [8 16 32 16 8] INF)))

  (demo (free-verb (saw (midicps 32)) 3 500 200))
  (kill space-organ)

