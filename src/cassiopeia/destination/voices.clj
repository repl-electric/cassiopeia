(ns cassiopeia.destination.voices
  "
  _|      _|            _|
  _|      _|    _|_|          _|_|_|    _|_|      _|_|_|
  _|      _|  _|    _|  _|  _|        _|_|_|_|  _|_|
    _|  _|    _|    _|  _|  _|        _|            _|_|
      _|        _|_|    _|    _|_|_|    _|_|_|  _|_|_|

Hearing Voices From Space.
A space suit floats through the empty void and orbits our planet, its radio broadcasts signals to the Earth below. On the surface, amateur radio operators receive the transmission and hear a voice. Coming from the suit, as it floats alone, away from the international space station.
  "
  (:use overtone.live)
  (:require [cassiopeia.engine.timing :as tim]))

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
        src (dyn-klank:ar [freqs envs rings] src)
        src (* src (env-gen:kr (env-lin (rand time) (/ time 3) (rand time))))
        src (hpf:ar src 120)

        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])
        src (delay-c:ar src 0.4 [(rand 0.4) (rand 0.4) 1/8 src])]
    (out out-bus (* amp src))))

(ringing [:head ring-g] :amp 0.02)
(kill ring-g)

(defsynth noise-ocean [amp 1 out-bus 0]
  (let [src (one-pole:ar (+ (* 0.5 (dust:ar 100)) (* 0.1 (white-noise:ar))) 0.7)
        src (+ src (splay:ar (freq-shift:ar src [1/4 1/5 1/6 1/7])))]
    (out out-bus (* amp src))))

(comment
  (noise-ocean)
  (stop))

(defsynth dark-ambience [i 0 out-bus 0 amp 1 mul 0.2 room-size 70 rev-time 99]
  (let [a (hpf:ar (* (* 5e-3 (pink-noise)) (line:kr 0 1 9)) 10)
        src1 (ringz (* a (lf-noise1:kr (+ 0.05 0.1))) (+ 60 (* 55 0)) mul)
        src2 (ringz (* a (lf-noise1:kr (+ 0.05 0.1))) (+ 60 (* 55 1)) mul)
        src3 (ringz (* a (lf-noise1:kr (+ 0.05 0.1))) (+ 60 (* 55 2)) mul)
        src4 (ringz (* a (lf-noise1:kr (+ 0.05 0.1))) (+ 60 (* 55 3)) mul)
        src5 (ringz (* a (lf-noise1:kr (+ 0.05 0.1))) (+ 60 (* 55 4)) mul)
        src (tanh (g-verb (sum [src1 src2 src3 src4 src5]) room-size rev-time))]
    (out out-bus (* amp src))))

(def dark (dark-ambience :mul 0.2 :amp 0.2))

(ctl dark :mul 0.2 :room-size 70)
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

(defsynth sistres-2 [out-bus 0 amp 1 note 72]
  (let [h (midicps note)
        sins (take 16 (repeatedly #(* 0.2 (sin-osc:ar (exp-rand h (+ h (/ h 128))) 0))))
        src (* sins (lf-gauss:ar 6 1/4 0 0 2))]
    (out out-bus (* amp (splay:ar src)))))

(sistres-2 :note (rand-nth (rand-nth sis-score)))
(sistres)

(def dur-size 3)
(defonce perc-dur-buf  (buffer dur-size))
(defonce perc-amp-buf  (buffer dur-size))
(defonce post-frac-buf (buffer dur-size))

(defonce perc-g (group "perc grouping"))

(defsynth buf->perc-int [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0]
  (let [cnt (in:kr tim/beat-count-b)
        ;;beat-trg (in:kr tim/beat-b)
        dur (buf-rd:kr 1 perc-dur-buf (mod cnt 3))
        cutom-amp (buf-rd:kr 1 perc-amp-buf (mod cnt 3))
        pos-frac (buf-rd:kr 1 post-frac-buf (mod cnt 3))

        bar-trg  (= beat-num (mod cnt 3))
        amp      (set-reset-ff bar-trg)

        width-frac (* (/ dur (buf-dur:ir buf)) rate)
        sig [(buf-rd:ar 1 buf (phasor:ar 0
                                         (* rate (buf-rate-scale:kr buf))
                                         (* pos-frac (buf-samples:kr buf))
                                         (+ (* pos-frac (buf-samples:kr buf) )
                                            (* width-frac (buf-samples:kr buf))))
                        true
                        inter)]
        env (env-gen:kr (env-perc) bar-trg 1 0 dur)
        ]
    (out:ar out-bus (* env amp cutom-amp sig))))


(defonce smooth-g (group "smooth grouping"))

(defonce smooth-dur-buf  (buffer dur-size))
(defonce smooth-amp-buf  (buffer dur-size))
(defonce smooth-post-frac-buf  (buffer dur-size))

(defsynth buf->smooth-int [out-bus 0 buf [0 :ir] rate 1 inter 2 beat-num 0]
  (let [cnt (in:kr tim/beat-count-b)
        ;;beat-trg (in:kr tim/beat-b)
        dur (buf-rd:kr 1 smooth-dur-buf (mod cnt 3))
        custom-amp (buf-rd:kr 1 smooth-amp-buf (mod cnt 3))
        pos-frac (buf-rd:kr 1 post-frac-buf (mod cnt 3))
        bar-trg  (= beat-num (mod cnt 3))
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

(def space-and-time-sun (load-sample "~/Workspace/music/samples/space_and_time.wav"))
(def example-s (load-sample "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/sounds/a11wlk01.wav"))

(defn make-perc [lib]
  (doall
   (map
    (fn [n]
      (buf->perc-int
       [:head perc-g]
       :buf (rand-nth lib)
       :rate 3
       :pos-frac (/ (rand 512) 512)
       :beat-num n
       :amp (rand-nth (range 1 3)))
      perc-g)
   (range 0 3))))

(defn make-smooth [lib]
  (doall
   (map
    (fn [n]
      (buf->smooth-int
       [:head smooth-g]
       :buf (rand-nth lib)
       :rate 1
       :pos-frac (/ (rand 512) 512)
       :beat-num n
       :amp (rand-nth (range 1 3)))))
    (range 0 3)))

(buffer-write! perc-dur-buf [1/8 1/4 1/2])

(buffer-write! perc-amp-buf [(ranged-rand 1 3) (ranged-rand 1 3) (ranged-rand 1 3)])
(buffer-write! post-frac-buf [(/ (rand 512) 512) (/ (rand 512) 512) (/ (rand 512) 512)])

(buffer-write! smooth-dur-buf [1/2 1/8 1/2])
(buffer-write! smooth-amp-buf [(ranged-rand 1 3) (ranged-rand 1 3) (ranged-rand 1 3)])
(buffer-write! smooth-post-frac-buf [(/ (rand 512) 512) (/ (rand 512) 512) (/ (rand 512) 512)])

(def gs (make-perc [example-s]))
(def ss (make-smooth [space-and-time-sun example-s]))

(kill smooth-g)
(kill perc-g)

(stop)
