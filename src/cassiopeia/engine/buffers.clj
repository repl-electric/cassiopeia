(ns cassiopeia.engine.buffers
  (:use
   [overtone.live]
   [overtone.helpers audio-file lib file doc]))

(defn buffer-mix-to-mono [b]
  (ensure-buffer-active! b)
  (let [n-chans (:n-channels b)
        rate    (:rate b)]
    (cond
     (= 1 n-chans) b
     :else
     (let [data          (buffer-data b)
           partitioned   (partition n-chans (seq data))
           mixed         (mapv (fn [samps] (/ (apply + samps) n-chans)) partitioned)
           tmp-file-path (mk-path (mk-tmp-dir!) "mono-file.wav")]

       (write-wav mixed tmp-file-path rate 1)
       (let [new-b (buffer-alloc-read tmp-file-path)]
         (future (rm-rf! tmp-file-path))
         new-b)))))
