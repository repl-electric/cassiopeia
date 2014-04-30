(ns cassiopeia.waves.soprano
  (:use overtone.live))

(defonce silent-buffer (buffer 0))

(def soprano-root-dir "/Users/josephwilk/Workspace/music/samples/soprano/Samples/Sustains")

(defonce mm-low-s (load-sample (str soprano-root-dir "/Mm p/vor_sopr_sustain_mm_p_03.wav")))
(defonce mm-high-s (load-sample (str soprano-root-dir "/Mm p/vor_sopr_sustain_mm_p_04.wav")))

(defonce soprano-samples
  (doall (map (fn [idx note] [note (load-sample
                                   (str soprano-root-dir (format "/Ee F/vor_sopr_sustain_ee_F_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72 73 74])))

(defonce ah-strong-soprano-samples
  (doall (map (fn [idx note] [note (load-sample
                                   (str soprano-root-dir (format "/Ah F/vor_sopr_sustain_ah_F_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72 73 74])))

(defonce ah-soprano-samples
  (doall (map (fn [idx note] [note (load-sample
                                   (str soprano-root-dir (format "/Ah p/vor_sopr_sustain_ah_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72 73 74])))

(defonce yeh-soprano-samples
  (doall (map (fn [idx note] [note (load-sample
                                   (str soprano-root-dir (format "/Yeh p/vor_sopr_sustain_eh_p_%02d" idx) ".wav"))]) (range 1 14) [60 61 62 63 64 65 66 67 68 69 70 71 72  73  74])))

(defonce index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defonce ah-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] ah-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defonce ah-strong-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] ah-strong-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defonce yeh-index-buffer
  (let [b (buffer 128)]
    (buffer-fill! b (:id silent-buffer))
    (doseq [[note val] yeh-soprano-samples]
      (buffer-set! b note (:id val)))
    b))

(defsynth sing [out-bus 0 note 60 amp 0.1 pos 0]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr :attack 2 :release 1 :sustain 1 :decay 0.2) :action FREE )]
    (out 0 (* env amp (pan2 (scaled-play-buf 1 buf) pos)))))

(defsynth fast-singer [note-buf 0 amp 1 pos 0 attack 1 release 0.4 count-b 0 beat-b 0]
  (let [cnt (in:kr count-b)
        trg (in:kr beat-b)
        note (buf-rd:kr 1 note-buf cnt)
        buf (index:kr (:id index-buffer) note)
        env (env-gen (perc :release release :attack attack) :gate trg)]
    (out 0 (* env amp (pan2 (scaled-play-buf 1 buf :rate 1 :trigger trg) pos)))))

(defsynth fast-varied-singer [note-buf 0 amp 1 pos 0 release 0.4 count-b 0 beat-b 0 attack-b 0]
  (let [cnt (in:kr count-b)
        trg (in:kr beat-b)
        note (buf-rd:kr 1 note-buf cnt)
        attack (buf-rd:kr 1 attack-b cnt)
        buf (index:kr (:id index-buffer) note)
        env (env-gen (perc :release release :attack attack) :gate trg)]
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
