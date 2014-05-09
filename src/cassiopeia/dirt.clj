(ns cassiopeia.dirt
  "Easy access to DIRT's samples: https://github.com/yaxu/Dirt.
   Clone the repo and port `dirt-home` to it."
  (:use [overtone.live] [clojure.java.io] [overtone.helpers.file])
 (:import java.io.File)
  (:import java.io.FileNotFoundException))

(defonce dirt-home (resolve-tilde-path "~/Workspace/music/Dirt/samples/"))
(defonce samples-cache (atom {}))

(defn- walk [^File dir]
  (let [children (.listFiles dir)
        subdirs (filter #(.isDirectory %) children)
        files (filter #(.isFile %) children)]
    (concat files (mapcat walk subdirs))))

(defn dirt
  "Fetches and caches locally dirt samples. All dirt samples are refered to by a containing folder `sample-name` and an int `n` which specifies which file to play. Only wav + aiff are supported by Overtone so ignore anything else.
   Example:
    (sample-player (dirt :amp 0))"
  ([sample-name] (dirt sample-name 1))
  ([sample-name n]
     (if-let [sample (@samples-cache (str (name sample-name) ":" n))]
       sample
       (let [sample-name (name sample-name)
             samples (->> (walk (file (str dirt-home sample-name "/")))
                          (filter #(re-find #"(?i)wav|aiff" (or (file-extension %1) ""))))
             n (if (>= n (count samples)) 0 n)
             sample-file (nth samples n)]
         (when sample-file
           (swap! samples-cache assoc (str sample-name ":" n) (load-sample sample-file))
           (@samples-cache (str sample-name ":" n)))))))

(comment
  ;;Some example choices:
  (sample-player (dirt (rand-nth [:ab :ade :ades2 :ades3 :ades4 :alex :alphabet :amencutup :armora :arp :arpy :auto :baa :baa2 :bass :bass0 :bass1 :bass2 :bass3 :bassdm :bassfoo :battles :bd :bend :bev :bin :birds3 :bleep :blip :blue :bottle :breaks125 :breaks152 :breaks152loud :breaks152louder :breaks157 :breaks165 :breath :bubble :can :casio :cc :chin :chink :circus :clak :click :co :cosmicg :cp :cr :crow :d :db :diphone :diphone2 :dist :dork2 :dorkbot :dr :dr2 :dr55 :dr_few :drum :drumtraks :e :east :electro1 :erk :f :feel :feelfx :fest :fire :flick :foo :fuckable :future :gab :gabba :gabbaloud :gabbalouder :glasstap :glitch :glitch2 :gretsch :h :hand :hardcore :haw :hc :hh :hh27 :hit :hmm :ho :house :ht :if :ifdrums :incoming :industrial :insect :invaders :jazz :jungbass :jungle :jvbass :koy :kurt :latibro :led :less :lighter :lt :made :made2 :mash :mash2 :metal :miniyeah :moan :monsterb :moog :mouth :mp3 :msg :mt :mute :newnotes :noise :noise2 :notes :numbers :oc :odx :off :pad :padlong :pebbles :perc :peri :pluck :print :printshort :proc :procshort :psr :rave :rave2 :ravemono :rm :sax :seawolf :sequential :sf :sheffield :short :sid :sine :sitar :sn :space :speech :speechless :speedupdown :stab :stomp :subroc3d :sugar :sundance :tabla :tabla2 :tablex :tacscan :tech :techno :tink :tok :toys :trump :ul :ulgab :uxay :v :voodoo :wind :wobble :world :xmas :yeah]) (rand-int 100)) :rate 1;;(ranged-rand 0.1 1.0)
                 )

  (sample-player (dirt (rand-nth [:808 :bass2 :bass1 :bass0 :bend :bev :birds3]) (rand-int 100)))
  )
