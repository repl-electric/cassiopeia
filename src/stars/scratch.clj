(ns stars.scratch
  (:use [overtone.live]
        [launchpad.core])
  (:require
   [launchpad.plugin.sample-rows :refer :all]
   [launchpad.device :as device]
   [launchpad.state-maps :as state-maps]
   [launchpad.sequencer :refer :all]
   [overtone.at-at :as at-at]))

(def lp (first launchpad-kons))

(def phat-s        (sample (freesound-path 48489)))
(def groove-s      (sample (freesound-path 48488)))
(def funky-s       (sample (freesound-path 172549)))
(def memory-moon-s (sample (freesound-path 27567)))
(def retweak-s     (sample (freesound-path 25921)))

(defsynth reverb-skipping-sequencer
  [buf 0 rate 1 out-bus 0 start-point 0 bar-trg [0 :tr] loop? 0 vol 1.0 pan 0 rot 1 room 0.5 wet 0.83 damp 0.8 mix 0.83]
  (let [p (scaled-play-buf 1 buf rate bar-trg start-point loop?)]
    (out [0 1]
         (* vol (free-verb p mix room damp)))))

(def phat        (reverb-skipping-sequencer :buf (to-sc-id phat-s) :loop? true :bar-trg 0 :out-bus 0 :vol 0))
(def groove      (reverb-skipping-sequencer :buf (to-sc-id groove-s) :loop? true :bar-trg 0 :out-bus 0 :vol 0))
(def funky       (skipping-sequencer :buf (to-sc-id funky-s) :loop? true :bar-trg 0 :out-bus 0 :vol 0))
(def memory-moon (skipping-sequencer :buf (to-sc-id memory-moon-s) :loop? true :bar-trg 0 :out-bus 0 :vol 0))
(def retweak     (skipping-sequencer :buf (to-sc-id retweak-s) :loop? true :bar-trg 0 :out-bus 0 :vol 0))

(def phat-row        {:playtime (atom 0) :start (atom nil) :row 0 :sample phat-s        :sequencer phat})
(def groove-row      {:playtime (atom 0) :start (atom nil) :row 1 :sample groove-s      :sequencer groove})
(def funky-row       {:playtime (atom 0) :start (atom nil) :row 2 :sample funky-s       :sequencer funky})
(def memory-moon-row {:playtime (atom 0) :start (atom nil) :row 3 :sample memory-moon-s :sequencer memory-moon})
(def retweak-row     {:playtime (atom 0) :start (atom nil) :row 4 :sample retweak-s     :sequencer retweak})

(sample-rows lp :left [phat-row groove-row funky-row memory-moon-row retweak-row])

  ;;Playing

  (comment
    (ctl groove :out-bus 0 :vol 1.0)
    (ctl phat   :out-bus 0 :vol 1.0 :rate 1)
    (ctl groove :pan -1)
    (ctl groove :pan 1)

    (ctl groove :rate 1)

    (ctl phat :mix 1 :room 0.8 :damp 0.8)
    (ctl phat :mix 0 :room 0 :damp 0)
    (ctl groove :mix 0 :room 0 :damp 0)

    (ctl phat :room 0.8)
    (ctl phat :wet 0.0 :room 0)
    (ctl phat :rot 1))

;;(stop)
;;(stop-all)
