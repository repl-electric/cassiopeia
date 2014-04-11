(ns cassiopeia.waves.soprano
  (:use overtone.live))

(defonce silent-buffer (buffer 0))

(defonce silent-buffer (buffer 0))

(def soprano-samples (doall (map (fn [idx note] [note (load-sample
                                                       (str (format "/Users/josephwilk/Workspace/music/samples/soprano/Samples/Sustains/Ee F/vor_sopr_sustain_ee_F_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(def ah-strong-soprano-samples (doall (map (fn [idx note] [note (load-sample
                                                                 (str (format "/Users/josephwilk/Workspace/music/samples/soprano/Samples/Sustains/Ah F/vor_sopr_sustain_ah_F_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(def ah-soprano-samples (doall (map (fn [idx note] [note (load-sample
                                                          (str (format "/Users/josephwilk/Workspace/music/samples/soprano/Samples/Sustains/Ah p/vor_sopr_sustain_ah_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(def yeh-soprano-samples (doall (map (fn [idx note] [note (load-sample
                                                           (str (format "/Users/josephwilk/Workspace/music/samples/soprano/Samples/Sustains/Yeh p/vor_sopr_sustain_eh_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))


(def index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(def ah-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] ah-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(def ah-strong-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] ah-strong-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(def yeh-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] yeh-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defsynth sing [out-bus 0 note 60 amp 0.1 pos 0]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr :attack 2 :release 1 :sustain 1 :decay 0.2) :action FREE )]
    (out 0 (* env amp (pan2 (scaled-play-buf 1 buf) pos)))))

(defsynth fast-singer [note-buf 0 amp 1 pos 0 release 0.2 count-b 0 beat-b 0]
  (let [cnt (in:kr count-b)
        trg (in:kr beat-b)
        note (buf-rd:kr 1 note-buf cnt)
        buf (index:kr (:id index-buffer) note)
        env (env-gen (perc :release 0.4 :attack 1) :gate trg)]
    (out 0 (* env amp (pan2 (scaled-play-buf 1 buf :rate 1 :trigger trg) pos)))))

(defsynth slow-singer [note-buf 0 amp 1 pos 0 release 0.2 count-b 0 beat-b 0 seq-b 0 beat-num 0 num-steps 6
                       attack 0.2 release 6 decay 0.09 index-b 0]
  (let [cnt      (in:kr count-b)
        beat-trg (in:kr beat-b)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-b cnt)
                     (= beat-num (mod cnt num-steps))
                     beat-trg)
        buf (index:kr index-b note)
        env (env-gen (adsr :attack attack :sustain 6 :release release :decay decay) :gate bar-trg)]
    (out 0 (* amp env (pan2 (scaled-play-buf 1 buf :rate 1 :trigger bar-trg) pos)))))
