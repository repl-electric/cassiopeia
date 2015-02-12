(ns cassiopeia.destination.yarra
  (:use [overtone.live][mud.core])
  (:require [mud.timing :as time]))


(ctl-global-clock 0.5)

(defsynth dark-river
  "Dark, rough and sharp sea horn.
     Note: we are purposely not using recusion using busses. Just does not have the same feel."
  [out_bus 0

   note-buf 0
   note_slide 0
   note_slide_shape 5
   note_slide_curve 0

   beat-bus (:count time/beat-4th)  beat-trg-bus (:beat time/beat-4th)

   pan 0
   pan_slide 0
   pan_slide_shape 5
   pan_slide_curve 0

   amp 1
   amp_slide 0
   amp_slide_shape 5
   amp_slide_curve 0

   attack 1
   decay 0
   sustain 0
   release 4.0
   attack_level 1
   sustain_level 1
   env_curve 2]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        note note ;;(varlag note note_slide note_slide_curve note_slide_shape)
        amp amp;;(varlag amp amp_slide amp_slide_curve amp_slide_shape)
        pan  pan ;;(varlag pan pan_slide pan_slide_curve pan_slide_shape)
        freq (midicps note)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* (lf-noise1:ar 0.1) 3))))

        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        a (tanh (* 6 (lf-noise1:ar 3) (sin-osc:ar freq (* a (lf-noise1:ar 0.1) 3))))
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (allpass-l:ar a 0.3 [(+ (ranged-rand 0 0.2) 0.1) (+ (ranged-rand 0 0.2) 0.1)] 5)
        a (tanh a)

        env (env-gen:kr (env-adsr attack decay sustain release attack_level sustain_level env_curve) :action FREE)
        snd (* amp a)]
    (out out_bus (* env (pan2 snd pan)))))


(defonce d-notes-b (buffer 256))
(def river-flowing  (dark-river :note-buf d-notes-b))

(def nz [33, 35, 37, 40, 42, 45, 47, 49, 52, 54, 57])

(ctl river-flowing :release 6.0 :attack 3.0)
(pattern! d-notes-b (shuffle nz))

(def trigger-g101518
  (on-beat-trigger 1 #(do
                       (ctl river-flowing :release 6.0 :attack 3.0)
                       )))

(remove-beat-trigger trigger-g101518)
(remove-all-beat-triggers)


(def trigger-g101517
  (on-beat-trigger 32 #(do
                       (ctl river-flowing :release 4.0 :attack 2.0)
                       )))

(remove-beat-trigger trigger-g101517)
(remove-all-beat-triggers)




(stop)
