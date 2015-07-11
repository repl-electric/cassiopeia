(ns cassiopeia.engine.samples
  (:use overtone.live))

(def SAMPLE-ROOT "/Users/josephwilk/Dropbox/repl-electric/samples/")

(defn load-local-sample     [sample] (load-sample (str SAMPLE-ROOT sample)))
(defn local-recording-start [name]   (recording-start (str SAMPLE-ROOT name)))

(defonce directory (clojure.java.io/file "/Users/josephwilk/Workspace/music/samples/"))
(defonce files (file-seq directory))

(defn find-sample [match idx]
  (let [r (->> (filter #(and
                         (re-find #"\.wav" (.getName %))
                         (re-find (re-pattern (str "(?i)" match)) (.getName %))) files)
               (map #(.getAbsolutePath %1)))]
    (nth r (mod idx (count r)))))
