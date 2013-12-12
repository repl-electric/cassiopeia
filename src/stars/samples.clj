(ns stars.samples
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
(defonce bell-s              (sample (freesound-path 173000)))

(defonce click-s    (sample (freesound-path 406)))
(defonce boom-s     (sample (freesound-path 33637)))
(defonce subby-s    (sample (freesound-path 25649)))
(defonce choir-s    (sample (freesound-path 172323)))
(defonce godzilla-s (sample (freesound-path 206078)))
(defonce outiuty-s  (sample (freesound-path 55086)))

(def phat-s        (sample (freesound-path 198924)))
(def groove-s      (sample (freesound-path 48488)))
(def funky-s       (sample (freesound-path 172549)))
(def memory-moon-s (sample (freesound-path 27567)))
(def retweak-s     (sample (freesound-path 25921)))

(def heavy-bass-kick-a-s (load-sample "~/Workspace/music/samples/A80509__chiitown__heavy-bass-kick.wav"))
