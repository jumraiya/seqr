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

(defn- mk-listener []
  (let [local-counter (volatile! 0)]
    (proxy [Thread] ["counter-listener"]
      (run []
        (try
          (while (not (:terminate? @sequencer/state))
            (if (:running? @sequencer/state)
              (if (= @local-counter @sequencer/counter)
                (Thread/onSpinWait)
                (do
                  (vreset! local-counter @sequencer/counter)
                  (editor/highlight-action @local-counter)))
              (LockSupport/park this)))
          (catch Exception e
            (prn "Error in counter listener" e)))))))

(defn- resume-listener []
  (if (nil? counter-listener)
    (alter-var-root (var counter-listener)
                    (constantly (doto (mk-listener)
                                  (.start))))
    (condp = (.getState counter-listener)
      Thread$State/NEW (.start ^Thread counter-listener)
      Thread$State/RUNNABLE (println "counter listener already running")
      Thread$State/WAITING (LockSupport/unpark counter-listener)
      Thread$State/TIMED_WAITING (println "counter listener is timed waiting")
      Thread$State/BLOCKED (println "counter listener is blocked")
      Thread$State/TERMINATED
      (alter-var-root (var counter-listener)
                      (constantly (mk-listener))))))

(defn mk-bar []
  (let [play-btn (JButton. "Start")]
    (utils/add-action-listener
        play-btn
        (try
          (sequencer/start|pause)
          (if (sequencer/is-running?)
            (do
              (resume-listener)
              (.setText play-btn "Pause"))
            (.setText play-btn "Start"))
          (catch Exception e)))
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
      (.add play-btn))))
