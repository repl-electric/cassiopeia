(ns cassiopeia.waves.buf-effects)

(defsynth schroeder-reverb
  [rate 1 buf 0]
  (let [input    (pan2 (play-buf 1 buf rate :action FREE) -0.5)
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))]
    (out 0 output)))

(defsynth schroeder-reverb-mic
  [rate 1 dec 1 del 10 out-bus 0]
  (let [input    (pan2 (allpass-c (sound-in) 10  dec del))
        delrd    (local-in 4)
        output   (+ input [(first delrd) (second delrd)])
        sig      [(+ (first output) (second output)) (- (first output) (second output))
                  (+ (nth delrd 2) (nth delrd 3)) (- (nth delrd 2) (nth delrd 3))]
        sig      [(+ (nth sig 0) (nth sig 2)) (+ (nth sig 1) (nth sig 3))
                  (- (nth sig 0) (nth sig 2)) (- (nth sig 0) (nth sig 2))]
        sig      (* sig [0.4 0.37 0.333 0.3])
        deltimes (- (* [101 143 165 177] 0.001) (control-dur))
        lout     (local-out (delay-c sig deltimes deltimes))]
    (out out-bus output)))
