(defproject stars "0.1.0-SNAPSHOT"
  :description "Repl Electric: Cassiopeia"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

:dependencies [[org.clojure/clojure "1.5.1"]
               [overtone "0.9.1"]

               [korg-nano-kontrol2 "0.1.0-SNAPSHOT"]
               [launchpad "0.1.0-SNAPSHOT"]
               [monome "0.1.0-SNAPSHOT"]

               [overtone.synths "0.1.0-SNAPSHOT"]
               [overtone.orchestra "0.1.0-SNAPSHOT"]]

:jvm-opts [
;;             "-agentpath:/Applications/YourKit_Java_Profiler_12.0.5.app/bin/mac/libyjpagent.jnilib"
    "-Xms512m" "-Xmx1g"           ; Minimum and maximum sizes of the
                                  ; heap
    "-XX:+UseParNewGC"            ; Use the new parallel GC in
                                  ; conjunction with
    "-XX:+UseConcMarkSweepGC"     ;  the concurrent garbage collector
    "-XX:+CMSConcurrentMTEnabled" ; Enable multi-threaded concurrent
                                  ; gc work (ParNewGC)
    "-XX:MaxGCPauseMillis=20"     ; Specify a target of 20ms for max
                                  ; gc pauses
    "-XX:+CMSIncrementalMode"     ; Do many small GC cycles to
                                  ; minimize pauses
    "-XX:MaxNewSize=257m"         ; Specify the max and min size of
                                  ; the new
    "-XX:NewSize=256m"            ;  generation to be small
    "-XX:+UseTLAB"                ; Uses thread-local object
                                  ; allocation blocks. This
                                  ;  improves concurrency by reducing
                                  ;  contention on
                                  ;  the shared heap lock.
    "-XX:MaxTenuringThreshold=0"
;;    "-XX:+PrintGC"                ; Print GC info to stdout
;;    "-XX:+PrintGCDetails"         ;  - with details
;;    "-XX:+PrintGCTimeStamps"
    ] ; Makes the full NewSize available to
                                  ;  every NewGC cycle, and reduces
                                  ;  the
                                  ;  pause time by not evaluating
                                  ;  tenured objects. Technically,
                                  ;  this
                                  ;  setting promotes all live objects
                                  ;  to the older generation, rather
                                        ;  than copying them.
)
