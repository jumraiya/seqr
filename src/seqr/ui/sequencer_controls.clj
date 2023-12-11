(ns seqr.sequencer-controls
  (:require
   [seqr.ui.utils :as utils]
   [seqr.sequencer :as sequencer])
  (:import
   (javax.swing JButton JLabel JPanel JTextField)
   (java.awt FlowLayout)))

(defn mk-bar []
  (let [play-btn (JButton. "Start")]
    (utils/add-action-listener
     play-btn
     (try
       (sequencer/start|pause)
       (if (sequencer/is-running?)
         (.setText play-btn "Pause")
         (.setText play-btn "Start"))
       (catch Exception e)))
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
      (.add play-btn))))
