(ns stars.synths.synths
  (:use [overtone.core]))

(defsynth supersaw2 [freq 440 amp 2.5 fil-mul 2 rq 0.3 out-bus 0]
  (let [input  (lf-saw freq)
        shift1 (lf-saw 4)
        shift2 (lf-saw 7)
        shift3 (lf-saw 5)
        shift4 (lf-saw 2)
        comp1  (> input shift1)
        comp2  (> input shift2)
        comp3  (> input shift3)
        comp4  (> input shift4)
        output (+ (- input comp1)
                  (- input comp2)
                  (- input comp3)
                  (- input comp4))
        output (- output input)
        output (leak-dc:ar (* output 0.25))
        output (normalizer (rlpf output (* freq fil-mul) rq))]

    (out out-bus (* amp output (line 1 0 10 FREE)))))

(defsynth guitar
  [amp 1 out-bus 0 in-bus 1]
  (let [src (sound-in in-bus)]
    (out out-bus  (* amp src))))
