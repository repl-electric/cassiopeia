(ns cassiopeia.samples
  (:use
   [overtone.live]))

(defonce tom-electro-s     (sample (freesound-path 108001)))
(defonce high-hat-open-s   (sample (freesound-path 207914)))
(defonce hat-s             (sample (freesound-path 178663)))

(defonce heavy-bass-kick-s (sample (freesound-path 80509)))

(defonce snare-s             (sample (freesound-path 100397)))
(defonce kick-s              (sample (freesound-path 777)))
(defonce sizzling-high-hat-s (sample (freesound-path 44859)))
(defonce hip-hop-kick-s      (sample (freesound-path 131336)))
(defonce clap-s              (sample (freesound-path 24786)))

(def clap-s (freesound-sample 48310))
(def clap2-s (freesound-sample 132676))

(defonce bell-s              (sample (freesound-path 173000)))

(defonce shaker-s (sample (freesound-path 100008)))

(defonce click-s    (sample (freesound-path 406)))
(defonce boom-s     (sample (freesound-path 33637)))
(defonce subby-s    (sample (freesound-path 25649)))
(defonce choir-s    (sample (freesound-path 172323)))
(defonce godzilla-s (sample (freesound-path 206078)))
(defonce outiuty-s  (sample (freesound-path 55086)))

(defonce phat-s        (sample (freesound-path 198924)))
(defonce groove-s      (sample (freesound-path 48488)))
(defonce funky-s       (sample (freesound-path 172549)))
(defonce memory-moon-s (sample (freesound-path 27567)))
(defonce retweak-s     (sample (freesound-path 25921)))

(def beatbox-kick-s (freesound-sample 70631))

(def sample-root "~/Workspace/music/samples/sliced-p5/")

(defonce shake1-s (load-sample (str sample-root "single-shake.aif")))
(defonce shake2d-s (load-sample (str sample-root "double-shake-deep.aif")))
(defonce shake2-s (load-sample (str sample-root "double-shake.aif")))
(defonce shake-s (load-sample (str sample-root "shaker.aif")))
(defonce tom-s (load-sample (str sample-root "tom.aif")))
(defonce kick-s (load-sample (str sample-root "kick.aif")))

;;Custom samples
(def star-into-the-sun-s (load-sample "~/Workspace/music/samples/star-into-the-sun.wav"))
(def space-and-time-s (load-sample "~/Workspace/music/samples/space_and_time.wav"))
(def chaos-s (load-sample "~/Workspace/music/samples/chaos.wav"))

(defonce glitch1-s (freesound-sample 130301))
(defonce glitch2-s (freesound-sample 130304))
