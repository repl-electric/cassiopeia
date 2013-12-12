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

;;Samples by devism http://monome.org/community/discussion/736/x&page=1#Item_22
(def arp-s (load-sample "~/Workspace/music/samples/P5mlrARP.wav"))
(def arp-chord-s (load-sample "~/Workspace/music/samples/P5mlrARPCHORD.wav"))
(def chords-s (load-sample "~/Workspace/music/samples/P5mlrCHORDs.wav"))
(def voice-1-s (load-sample "~/Workspace/music/samples/P5mlrVOICE.wav"))
(def voice-2-s (load-sample "~/Workspace/music/samples/P5mlrVOICE2.wav"))
(def strings-s (load-sample "~/Workspace/music/samples/P5mlrSTRINGS.wav"))
(def drums-s (load-sample "~/Workspace/music/samples/P5mlrDRUMS.wav"))
(def dub-s (load-sample "~/Workspace/music/samples/P5mlrDUB.wav"))
(def string-s (load-sample "~/Workspace/music/samples/P5mlrSTRINGS.wav"))
(def bass-1-s (load-sample "~/Workspace/music/samples/P5mlrBASS1.WAV"))
(def bass-2-s (load-sample "~/Workspace/music/samples/P5mlrBASS2.WAV"))
(def bass-3-s (load-sample "~/Workspace/music/samples/P5mlrBASS3.WAV"))
(def hard-1-s (load-sample "~/Workspace/music/samples/P5mlrHARD1.WAV"))
(def hard-2-s (load-sample "~/Workspace/music/samples/P5mlrHARD2.WAV"))
(def hard-3-s (load-sample "~/Workspace/music/samples/P5mlrHARDDUB.WAV"))
(def gtr-1-s (load-sample "~/Workspace/music/samples/P5mlrGTR1.WAV"))
(def gtr-2-s (load-sample "~/Workspace/music/samples/P5mlrGTR2.WAV"))
(def gtr-3-s (load-sample "~/Workspace/music/samples/P5mlrGTR3.WAV"))
(def gtr-str-s (load-sample "~/Workspace/music/samples/P5mlrGTrSTr.wav"))
