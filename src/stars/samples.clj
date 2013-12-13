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



(defonce custom-kick-s (load-sample "~/Workspace/music/samples/kick.wav"))
(defonce custom-long-shake-s (load-sample "~/Workspace/music/samples/long-shake.wav"))
(defonce custom-shake-s (load-sample "~/Workspace/music/samples/shake.wav")
  )
(defonce custom-tom-shake-s (load-sample "~/Workspace/music/samples/tom-shake.wav"))
(defonce custom-fast-shake-s (load-sample "~/Workspace/music/samples/three-fast-shake.wav"))


;;Samples by devism http://monome.org/community/discussion/736/x&page=1#Item_22
(defonce arp-s (load-sample "~/Workspace/music/samples/P5mlr/ARP.wav"))
(defonce arp-chord-s (load-sample "~/Workspace/music/samples/P5mlr/ARPCHORD.wav"))
(defonce chords-s (load-sample "~/Workspace/music/samples/P5mlr/CHORDs.wav"))
(defonce voice-1-s (load-sample "~/Workspace/music/samples/P5mlr/VOICE.wav"))
(defonce voice-2-s (load-sample "~/Workspace/music/samples/P5mlr/VOICE2.wav"))
(defonce strings-s (load-sample "~/Workspace/music/samples/P5mlr/STRINGS.wav"))
(defonce drums-s (load-sample "~/Workspace/music/samples/P5mlr/DRUMS.wav"))
(defonce dub-s (load-sample "~/Workspace/music/samples/P5mlr/DUB.wav"))
(defonce string-s (load-sample "~/Workspace/music/samples/P5mlr/STRINGS.wav"))
(defonce bass-1-s (load-sample "~/Workspace/music/samples/P5mlr/BASS1.WAV"))
(defonce bass-2-s (load-sample "~/Workspace/music/samples/P5mlr/BASS2.WAV"))
(defonce bass-3-s (load-sample "~/Workspace/music/samples/P5mlr/BASS3.WAV"))
(defonce hard-1-s (load-sample "~/Workspace/music/samples/P5mlr/HARD1.WAV"))
(defonce hard-2-s (load-sample "~/Workspace/music/samples/P5mlr/HARD2.WAV"))
(defonce hard-3-s (load-sample "~/Workspace/music/samples/P5mlr/HARDDUB.WAV"))
(defonce gtr-1-s (load-sample "~/Workspace/music/samples/P5mlr/GTR1.WAV"))
(defonce gtr-2-s (load-sample "~/Workspace/music/samples/P5mlr/GTR2.WAV"))
(defonce gtr-3-s (load-sample "~/Workspace/music/samples/P5mlr/GTR3.WAV"))
(defonce gtr-str-s (load-sample "~/Workspace/music/samples/P5mlr/GTrSTr.wav"))
