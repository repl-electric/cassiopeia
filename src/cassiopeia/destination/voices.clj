(ns cassiopeia.destination.voices
  "
  _|      _|            _|
  _|      _|    _|_|          _|_|_|    _|_|      _|_|_|
  _|      _|  _|    _|  _|  _|        _|_|_|_|  _|_|
    _|  _|    _|    _|  _|  _|        _|            _|_|
      _|        _|_|    _|    _|_|_|    _|_|_|  _|_|_|
  "
  (:use overtone.live))

(defonce voice-g (group "the voices"))


(def score [24 28 29 48 36 40 41 52 53 60 64 65])

(defsynth voice [out-bus 0 note 0 pan 0 attack 10 release 10 mix 0.33]
  (let [freq (midicps note)
        sig (sum [(* (/ 0.05 (inc 0)) (var-saw:ar (* freq (+ 0 1.0001))))]
                 [(* (/ 0.05 (inc 1)) (var-saw:ar (* freq (+ 1 1.0001))))]
                 [(* (/ 0.05 (inc 2)) (var-saw:ar (* freq (+ 2 1.0001))))])
        sig2 (ringz:ar (* 0.0003 (white-noise:ar)) (t-rand:ar freq (midicps (+ 1 note)) (impulse:ar 10)))
        env (env-gen:kr (env-lin attack 1 release) 1 :action FREE)
        s (+ 0.8 (* 0.2 (sin-osc:kr 0.1 0)))
        src (* (+ sig sig2) s env)        src (limiter:ar (free-verb:ar (lpf:ar src 10000) :mix mix) 0.7)]
    (out out-bus (pan2:ar src pan))))

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
(defsynth ringing [amp 8 out-bus 0]
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

(buffer-write! ring-score-buf ring-score)

(ringing)
(kill ringing)
