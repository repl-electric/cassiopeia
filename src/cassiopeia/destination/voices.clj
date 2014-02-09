(ns cassiopeia.destination.voices
  "
  _|      _|            _|
  _|      _|    _|_|          _|_|_|    _|_|      _|_|_|
  _|      _|  _|    _|  _|  _|        _|_|_|_|  _|_|
    _|  _|    _|    _|  _|  _|        _|            _|_|
      _|        _|_|    _|    _|_|_|    _|_|_|  _|_|_|

Hearing Voices From Space.
A space suit floats through the empty void and orbits our planet, its radio broadcasts signals to the Earth below.
On the surface, amateur radio operators receive the transmission and hear a voice. Coming from the suit,
as it floats alone, away from the international space station.
  "
  (:use overtone.live)
  (:use cassiopeia.engine.samples)
  (:use cassiopeia.samples)
  (:use cassiopeia.warm-up)
  (:require [cassiopeia.engine.timing :as tim]
            [cassiopeia.engine.monome-sequencer :as mon]))

(defonce voice-g (group "the voices"))

(def score [24 28 29 48 36 40 41 52 53 60 64 65])

(defsynth voice [out-bus 0 note 0 pan 0 attack 10 release 10 mix 0.33 amp 1]
  (let [freq (midicps note)
        sig (sum [(* (/ 0.05 (inc 0)) (var-saw:ar (* freq (+ 0 1.0001))))]
                 [(* (/ 0.05 (inc 1)) (var-saw:ar (* freq (+ 1 1.0001))))]
                 [(* (/ 0.05 (inc 2)) (var-saw:ar (* freq (+ 2 1.0001))))])
        sig2 (ringz:ar (* 0.0003 (white-noise:ar)) (t-rand:ar freq (midicps (+ 1 note)) (impulse:ar 10)))
        env (env-gen:kr (env-lin attack 1 release) 1 :action FREE)
        s (+ 0.8 (* 0.2 (sin-osc:kr 0.1 0)))
        src (* (+ sig sig2) s env)
        src (limiter:ar (free-verb:ar (lpf:ar src 10000) :mix mix) 0.7)]
    (out out-bus (* amp (pan2:ar src pan)))))

(doseq [n (range 0 10)]
  (voice [:head voice-g]
         :note    (rand-nth score)
         :pan     (ranged-rand -0.5 0.5)
         :attack  (ranged-rand 5 13)
         :release (ranged-rand 8 14)))

(kill voice-g)
(stop)

(def num-voices 20)
(def ring-score (map midi->hz (take num-voices (cycle [26 40 54 67 81 95 109 110 124 138]))))
(defonce ring-score-buf (buffer num-voices))
(defonce ring-g (group "rings"))

(buffer-write! ring-score-buf ring-score)

(defsynth ringing [amp 1 out-bus 0]
  (let [time (rand 5)
        freqs ring-score

        rings (take num-voices (repeatedly #(rand 1.0)))
        envs (take num-voices (repeatedly #(env-gen (env-lin (/ time 3.0) (/ time 3.0) (/ time 3.0) (rand 1.0)))))

        src (* [(reciprocal num-voices) (reciprocal num-voices)] (pink-noise:ar))
        src (klank:ar [freqs envs rings] src)
        src (* src (env-gen:kr (env-lin (rand time) (/ time 3) (rand time))))
        src (hpf:ar src 120)

        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])]
    (out out-bus (* amp src))))

(ringing [:head ring-g] :amp 1)
(kill ring-g)

(defsynth noise-ocean [amp 1 out-bus 0]
  (let [src (one-pole:ar (+ (* 0.5 (dust:ar 100)) (* 0.1 (white-noise:ar))) 0.7)
        src (+ src (splay:ar (freq-shift:ar src [1/4 1/5 1/6 1/7])))]
    (out out-bus (* amp src))))

(comment
  (noise-ocean :amp 0.05)
  (kill noise-ocean))

(defsynth dark-ambience [out-bus 0 amp 1 mul 0.2 room-size 70 rev-time 99 ring-freq 60 ring-mul 55]
  (let [pink (hpf:ar (* (* 0.005 (pink-noise)) (line:kr 0 1 9)) 5)
        src1 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 0)) mul)
        src2 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 1)) mul)
        src3 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 2)) mul)
        src4 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 3)) mul)
        src5 (ringz (* pink (lf-noise1:kr 0.15)) (+ ring-freq (* ring-mul 4)) mul)
        src (tanh (g-verb (sum [src1 src2 src3 src4 src5]) room-size rev-time))]
    (out out-bus (* amp src))))

(def dark (dark-ambience :mul 0.2 :amp 0.2))

(ctl dark :ring-mul 55)
(ctl dark :ring-freq 600)
(ctl dark :mul 0.2 :room-size 70)
(ctl dark :mul 0.5 :rev-time 99)
(ctl dark :amp 0.1)
(ctl dark :room-size 70)

(kill dark)

(stop)

(def sis-score [[72 69 64]  [70 64 62]  [67 60 70]  [65 60 69]  [64 60 67] [65 60 69]])

(defsynth sistres [out-bus 0 amp 1]
  (let [h (midicps (rand-nth [40 45 52]))
        sins (take 16 (repeatedly #(* 0.2 (sin-osc:ar (exp-rand h (+ h (/ h 64))) 0))))
        src (* sins (lf-gauss:ar 9 1/4 0 0 2))]
    (out out-bus (* amp (splay:ar src)))))

(sistres :amp 10)

(def pattern-sizes [1 2 4 8 16 32 64 128 256])
(def durations [1/8 1/4 1/2 1])
(def voices 4)
(def pattern-size (rand-nth pattern-sizes))

(def perc-dur-buf  (buffer voices))
(defonce perc-amp-buf  (buffer pattern-size))
(defonce perc-post-frac-buf (buffer pattern-size))

(defonce perc-g (group "perc grouping"))

(defsynth buf->perc-inst [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0
                          pattern-buf 0 pattern-size 3
                          amp-buf 0
                          duration-buf 0
                          voices 3]
  (let [cnt (in:kr tim/beat-count-b)
        dur (buf-rd:kr 1 duration-buf (mod cnt voices))
        cutom-amp (buf-rd:kr 1 amp-buf (mod cnt pattern-size))
        pos-frac (buf-rd:kr 1 pattern-buf (mod cnt pattern-size))

        bar-trg  (= beat-num (mod cnt voices))
        amp      (set-reset-ff bar-trg)

        width-frac (* (/ dur (buf-dur:ir buf)) rate)
        sig [(buf-rd:ar 1 buf (phasor:ar 0
                                         (* rate (buf-rate-scale:kr buf))
                                         (* pos-frac (buf-samples:kr buf))
                                         (+ (* pos-frac (buf-samples:kr buf) )
                                            (* width-frac (buf-samples:kr buf))))
                        true
                        inter)]
        env (env-gen:kr (env-perc) bar-trg 1 0 dur)]
    (out:ar out-bus (pan2 (* env amp cutom-amp sig)))))


(defonce smooth-g (group "smooth grouping"))

(def smooth-dur-buf  (buffer voices))
(defonce smooth-amp-buf  (buffer pattern-size))
(defonce smooth-post-frac-buf  (buffer pattern-size))

(defsynth buf->smooth-inst [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0
                            pattern-buf 0 pattern-size 3
                            amp-buf 0
                            duration-buf 0
                            voices 3]
  (let [cnt (in:kr tim/beat-count-b)
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
        env (env-gen:kr (env-sine) bar-trg 1 0 dur)]
    (out out-bus (* env amp custom-amp sound))))

(defn make-perc
  ([lib] (make-perc lib 3))
  ([lib voices]
     (doall
      (map
       (fn [n]
         (buf->perc-inst
          [:head perc-g]
          :buf (rand-nth lib)
          :rate 1
          :beat-num n
          :pattern-size (buffer-size perc-post-frac-buf)
          :pattern-buf perc-post-frac-buf
          :amp-buf perc-amp-buf
          :duration-buf perc-dur-buf
          :voices voices)
         perc-g)
       (range 0 voices)))))

(defn make-smooth
  ([lib] (make-smooth lib 3))
  ([lib voices]
     (doall
      (map
       (fn  [n]
         (buf->smooth-inst
          [:head smooth-g]
          :buf (rand-nth lib)
          :rate 1
          :beat-num n
          :pattern-size (buffer-size smooth-post-frac-buf)
          :pattern-buf smooth-post-frac-buf
          :amp-buf smooth-amp-buf
          :duration-buf smooth-dur-buf
          :voices voices))
        (range 0 voices)))))

(defn resize-pattern [old-buf group new-size synth-ctl]
  (let [size (buffer-size old-buf)
        new-buf (buffer new-size)
        old-vals (buffer-read old-buf)]
    (buffer-write! new-buf (take new-size (cycle old-vals)))
    (ctl group synth-ctl new-buf :pattern-size new-size)
    (buffer-free old-buf)
    new-buf))

(defn spin-durations-for-voice [voice buf]
  (buffer-write! buf voice  [(rand-nth durations)]))

(def pattern-size (rand-nth pattern-sizes))

(def smooth-post-frac-buf (resize-pattern smooth-post-frac-buf smooth-g pattern-size :pattern-buf))
(def smooth-amp-buf       (resize-pattern smooth-amp-buf smooth-g pattern-size :amp-buf))

(buffer-write! smooth-dur-buf       (take voices (repeatedly #(do 1))))
(buffer-write! smooth-amp-buf       (take pattern-size (repeatedly #(ranged-rand 1 3))))
(buffer-write! smooth-post-frac-buf (take pattern-size (repeatedly #(/ (rand 512) 512))))

(def ss (make-smooth [constant-blues-s death-s dreamers-of-the-dreams-s] 4))

(def perc-post-frac-buf (resize-pattern perc-post-frac-buf perc-g pattern-size :pattern-buf))
(def perc-amp-buf       (resize-pattern perc-amp-buf perc-g pattern-size :amp-buf))

(buffer-write! perc-dur-buf       (take voices (repeatedly #(rand-nth durations))))
(buffer-write! perc-amp-buf       (take pattern-size (repeatedly #(ranged-rand 1 3))))
(buffer-write! perc-post-frac-buf (take pattern-size (repeatedly #(/ (rand 512) 512))))

(def gs (make-perc [constant-blues-s death-s dreamers-of-the-dreams-s afraid-s one-moment-please-s] voices))

(spin-durations-for-voice (rand-int voices) perc-dur-buf)
(spin-durations-for-voice (rand-int voices) smooth-dur-buf)

(kill smooth-g)
(kill perc-g)

(stop)

(comment
  ;;Part of chaos
  (def perc-amp (buffer-read perc-amp-buf))
  (def smooth-amp (buffer-read smooth-amp-buf))

  (buffer-write! perc-amp-buf   (take pattern-size (cycle [0])))
  (buffer-write! smooth-amp-buf (take pattern-size (cycle [0])))

  (buffer-write! perc-amp-buf [0.2 0.5 0.2 0.5])

  (buffer-write! perc-amp-buf perc-amp)
  (buffer-write! smooth-amp-buf perc-amp))
