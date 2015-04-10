(ns cassiopeia.destination.yarra
  (:use [overtone.live][mud.core])
  (:require [mud.timing :as time]))

(ctl-global-clock 1.0)

(defsynth dark-ambience
  [note 52
   note_slide 0
   note_slide_shape 5
   note_slide_curve 0

   beat-bus (:count time/beat-8th)  beat-trg-bus (:beat time/beat-8th)
   note-buf 0

   amp 1
   amp_slide 0
   amp_slide_shape 5
   amp_slide_curve 0

   pan 0
   pan_slide 0
   pan_slide_shape 5
   pan_slide_curve 0

   attack 0
   decay 0
   sustain 0
   release 4
   attack_level 1
   sustain_level 1
   env_curve 2

   cutoff 110
   cutoff_slide 0
   cutoff_slide_shape 5
   cutoff_slide_curve 0
   res 0.3
   res_slide 0
   res_slide_shape 5
   res_slide_curve 0

   detune1 12
   detune1_slide 0
   detune1_slide_shape 5
   detune1_slide_curve 0

   detune2 24
   detune2_slide 0
   detune2_slide_shape 5
   detune2_slide_curve 0

   noise 0
   ring 0.2
   room 70
   reverb_time 100
   out_bus 0]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        gate-trg (and (> note 0) beat-trg)

        note          (varlag note note_slide note_slide_curve note_slide_shape)
        amp           (varlag amp amp_slide amp_slide_curve amp_slide_shape)
        amp-fudge     1
        pan           (varlag pan pan_slide pan_slide_curve pan_slide_shape)
        cutoff        (varlag cutoff cutoff_slide cutoff_slide_curve cutoff_slide_shape)
        res           (varlag res res_slide res_slide_curve res_slide_shape)
        detune1       (varlag detune1 detune1_slide detune1_slide_curve detune1_slide_shape)
        detune2       (varlag detune2 detune2_slide detune2_slide_curve detune2_slide_shape)

        freq          (midicps note)
        freq2         (midicps (+ note detune1))
        freq3         (midicps (+ note detune2))
        cutoff-freq   (midicps cutoff)
        room          (max 0.1 (min 300 (abs room))) ;; stops synth killing scsynth
        env           (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :gate gate-trg)

        pn            (* 0.005 (pink-noise))
        bn            (* 0.002 (brown-noise))
        wn            (* 0.002 (white-noise))
        cl            (* 0.001 (clip-noise))
        gn            (* 0.001 (gray-noise))
        src-noise     (select noise [pn bn wn cl gn])

        src1          (ringz src-noise freq ring)
        src2          (ringz src-noise freq2 ring)
        src3          (ringz src-noise freq3 ring)
        src           (g-verb (* env (mix [src1 src1 src2 src3])) room reverb_time)
        src           (tanh src)
        [src-l src-r] (rlpf (* amp-fudge env src) cutoff-freq res)
        src           (balance2 src-l src-r pan amp)]
    (out out_bus src)))

(defsynth dark-sea-horn
  "Dark, rough and sharp sea horn.
     Note: we are purposely not using recusion using busses. Just does not have the same feel."
  [out_bus 0

   note 52
   note_slide 0
   note_slide_shape 5
   note_slide_curve 0

   pan 0
   pan_slide 0
   pan_slide_shape 5
   pan_slide_curve 0

   beat-bus (:count time/beat-8th)  beat-trg-bus (:beat time/beat-4th)

   note-buf 0

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
        gate-trg (and (> note 0) beat-trg)

        note (varlag note note_slide note_slide_curve note_slide_shape)
        amp (varlag amp amp_slide amp_slide_curve amp_slide_shape)
        pan (varlag pan pan_slide pan_slide_curve pan_slide_shape)
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

        env (env-gen:kr (env-adsr-ng attack decay sustain release attack_level sustain_level env_curve) :gate gate-trg)
        snd (* amp a)]  (out out_bus (* env (pan2 snd pan)))))

(defonce d-notes-b (buffer 256))
(def river-flowing  (dark-sea-horn :note-buf d-notes-b))
(def nz [33, 35, 37, 40, 42, 45, 47, 49, 52, 54, 57])
(pattern! d-notes-b (shuffle nz))

(ctl river-flowing :release 4.0 :attack 0.5)


(pattern! d-notes-b (degrees-seq [:A2 4 4 _ 6 _ 5]))

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


(def dark-b (buffer 256))
(def dark (dark-ambience :note-buf dark-b :amp 2))

(pattern! dark-b (degrees-seq [:A3 1 0 0 0
                               :A2 0 0 0 4
                               :A4 1 :A3 1 0 :A2 4
                               :A2 0 0 0 4
                               :A3 5 0 0 0
                               :A3 0 0 0 4]))

(stop)
