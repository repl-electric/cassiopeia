(ns cassiopeia.engine.samples
  (:use overtone.live))

(defonce __sample_cache__ (atom {}))
(def SAMPLE-ROOT "/Users/josephwilk/Dropbox/repl-electric/samples/")

(defn load-local-sample     [sample] (load-sample (str SAMPLE-ROOT sample)))
(defn local-recording-start [name]   (recording-start (str SAMPLE-ROOT name)))

(defonce directory-for-samples (clojure.java.io/file "/Users/josephwilk/Workspace/music/samples/"))
(defonce all-files-samples (file-seq directory-for-samples))
(defonce ether-set (file-seq (clojure.java.io/file "/Users/josephwilk/Workspace/music/samples/Ether")))
(defonce mountain-set (file-seq (clojure.java.io/file "/Users/josephwilk/Workspace/music/samples/Mountain")))

(defn find-sample
([match idx] (find-sample match idx all-files-samples))
([match idx sample-files]
     (let [sample-key (str match ":" idx)]
       (or (get sample-key @__sample_cache__)
           (let [r  (->> (filter #(and
                                   (re-find #"\.wav" (.getName %))
                                   (re-find (re-pattern (str "(?i)" match)) (.getName %))) sample-files)
                         (map #(.getAbsolutePath %1)))
                 sample (sample  (nth r (mod idx (count r))))]
             (swap! __sample_cache__ assoc sample-key sample)
             sample)))))
