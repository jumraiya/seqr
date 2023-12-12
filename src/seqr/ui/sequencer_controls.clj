(ns seqr.ui.sequencer-controls
  (:require
   [seqr.ui.utils :as utils]
   [seqr.ui.editor :as editor]
   [seqr.helper :as helper]
   [seqr.sequencer :as sequencer])
  (:import
   (javax.swing JButton JLabel JPanel JTextField)
   (java.awt FlowLayout)
   (java.util.concurrent.locks LockSupport)))

(defonce counter-listener nil)

#trace
(defn- tes [text-view table-view local-counter]
  (if (= @local-counter @sequencer/counter)
    (Thread/onSpinWait)
    (do
      (vreset! local-counter @sequencer/counter)
      (editor/highlight-action
       text-view table-view @local-counter))))

(defn- mk-listener [text-view table-view]
  (let [local-counter (volatile! 0)]
    (proxy [Thread] ["counter-listener"]
      (run []
        (try
          (while (not (:terminate? @sequencer/state))
            (if (:running? @sequencer/state)
              (tes text-view table-view local-counter)
              (LockSupport/park this)))
          (catch Exception e
            (prn "Error in counter listener" e)
            ))))))
#trace
(defn- resume-listener [text-view table-view]
  (if (nil? counter-listener)
    (alter-var-root (var counter-listener)
                    (constantly (doto (mk-listener text-view table-view)
                                  (.start))))
    (condp = (.getState counter-listener)
      Thread$State/NEW (.start ^Thread counter-listener)
      Thread$State/RUNNABLE (println "counter listener already running")
      Thread$State/WAITING (LockSupport/unpark counter-listener)
      Thread$State/TIMED_WAITING (println "counter listener is timed waiting")
      Thread$State/BLOCKED (println "counter listener is blocked")
      Thread$State/TERMINATED
      (alter-var-root (var counter-listener)
                      (constantly (mk-listener text-view table-view))))))

(defn mk-bar [text-view table-view]
  (let [play-btn (JButton. "Start")]
    (utils/add-action-listener
        play-btn
        (try
          (sequencer/start|pause)
          (if (sequencer/is-running?)
            (do
              (resume-listener text-view table-view)
              (.setText play-btn "Pause"))
            (.setText play-btn "Start"))
          (catch Exception e)))
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
      (.add play-btn))))
