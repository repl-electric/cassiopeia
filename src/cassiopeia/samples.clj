(ns cassiopeia.samples
  (:use
   [overtone.live]
   [cassiopeia.engine.samples]))

;;Kicks
(defonce fragmented-kick-s (freesound-sample 8098))
(defonce wood-kick-s (freesound-sample 169190))
(defonce virus-kick-s (freesound-sample 105677))
(defonce acoustic-kick-s (freesound-sample 102785))
(defonce analog-kick-s (freesound-sample 208430))
(defonce soft-kick-s (freesound-sample 99522))
(defonce glitchy-kick-s (freesound-sample 87262))
(defonce kick-s               (freesound-sample 777))
(defonce beatbox-kick-s (freesound-sample 70631))

(defonce tom-electro-s   (freesound-sample 108001))
(defonce high-hat-open-s (freesound-sample 207914))
(defonce hat-s           (freesound-sample 178663))

(defonce heavy-bass-kick-s  (freesound-sample 80509))

(defonce snare-s              (freesound-sample 100397))
(defonce sizzling-high-hat-s  (freesound-sample 44859))
(defonce hip-hop-kick-s       (freesound-sample 131336))
(defonce clap-s               (freesound-sample 24786))

(def clap-s  (freesound-sample 48310))
(def clap2-s (freesound-sample 132676))

(def vogel-clap-s (freesound-sample 62427))

(defonce bell-s               (freesound-sample 173000))

(defonce shaker-s  (freesound-sample 100008))

(defonce click-s    (freesound-sample 406))
(defonce boom-s      (freesound-sample 33637))
(defonce subby-s     (freesound-sample 25649))
(defonce choir-s     (freesound-sample 172323))
(defonce godzilla-s  (freesound-sample 206078))
(defonce outiuty-s   (freesound-sample 55086))

(defonce phat-s         (freesound-sample 198924))
(defonce groove-s       (freesound-sample 48488))
(defonce funky-s        (freesound-sample 172549))
(defonce memory-moon-s  (freesound-sample 27567))
(defonce retweak-s      (freesound-sample 25921))

(def sample-root "~/Workspace/music/samples/sliced-p5/")

(defonce shake1-s  (load-local-sample "sliced-p5/single-shake.aif"))
(defonce shake2d-s (load-local-sample "sliced-p5/double-shake-deep.aif"))
(defonce shake2-s  (load-local-sample "sliced-p5/double-shake.aif"))
(defonce shake-s   (load-local-sample "sliced-p5/shaker.aif"))
(defonce tom-s     (load-local-sample "sliced-p5/tom.aif"))
(defonce kick-s    (load-local-sample "sliced-p5/kick.aif"))

;;Custom samples
(defonce star-into-the-sun-s      (load-local-sample "star-into-the-sun.wav"))
(defonce space-and-time-s         (load-local-sample "space_and_time.wav"))
(defonce chaos-s                  (load-local-sample "chaos.wav"))
(defonce dreamers-of-the-dreams-s (load-local-sample "dreamer-of-the-dreams.wav"))
(defonce afraid-s                 (load-local-sample "afraid.wav"))
(defonce one-moment-please-s      (load-local-sample "1moment-clean.wav"))
(defonce constant-blues-s         (load-local-sample "constant-blues.wav"))
(defonce death-s                  (load-local-sample "oh-death.wav"))
(defonce moore-s                  (load-local-sample "patrick-moore.wav"))
(defonce signals-s                (load-local-sample "signals-from-outerspace.wav"))

;;Triggers
(defonce glitch1-s (freesound-sample 130301))
(defonce glitch2-s (freesound-sample 130304))
(defonce boom-s (freesound-sample 65477))
(defonce pulse-s (freesound-sample 27568))
(defonce crunch-woosh-s (freesound-sample 170521))
(defonce whisper-s (freesound-sample 193818))
(defonce large-low-boom-s (freesound-sample 130974))
(defonce footsteps-s (freesound-sample 149234))
(defonce collect-coin-s (freesound-sample 135936))
(defonce cin-boom-s (freesound-sample 160688))
(defonce mystical-aura-s (freesound-sample 166185))

(defonce snare-ghost-s (freesound-sample 46567))
(defonce electric-woosh-s (freesound-sample 34172))
(defonce deep-bass-kick-s (freesound-sample 46528))
