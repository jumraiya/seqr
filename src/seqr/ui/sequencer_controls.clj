(ns seqr.ui.sequencer-controls
  (:require
   [seqr.ui.utils :as utils]
   [seqr.ui.editor :as editor]
   [seqr.helper :as helper]
   [seqr.midi :as midi]
   [seqr.sequencer :as sequencer])
  (:import
   (javax.swing JButton JLabel JPanel JTextField JComboBox DefaultComboBoxModel)
   (java.awt FlowLayout)
   (java.util.concurrent.locks LockSupport)))

(defonce counter-listener nil)

(defonce midi-input-list (atom nil))

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

(defn- mk-midi-device-list-model []
  (proxy [DefaultComboBoxModel] []
    (getSize []
      (count (midi/get-available-devices)))
    (getElementAt [idx]
      (nth (midi/get-available-devices) idx))))

(defn mk-bar []
  (let [play-btn (JButton. "Start")
        midi-label (JLabel. "Input MIDI Device")
        midi-inputs (reset! midi-input-list (doto (JComboBox. (mk-midi-device-list-model))
                                              (.setEditable false)))
        toggle-midi (JButton. "Record MIDI")]
    (utils/add-action-listener play-btn
                               (try
                                 (sequencer/start|pause)
                                 (if (sequencer/is-running?)
                                   (do
                                     (resume-listener)
                                     (.setText play-btn "Pause"))
                                   (.setText play-btn "Start"))
                                 (catch Exception e)))
    (utils/add-action-listener toggle-midi
                               (try
                                 (let [recording? (midi/toggle-recording
                                                   (.getSelectedItem @midi-input-list))]
                                   (if recording?
                                     (.setText toggle-midi "Stop Recording")
                                     (.setText toggle-midi "Record MIDI")))
                                 (catch Exception e)))
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
      (.add play-btn)
      (.add midi-label)
      (.add midi-inputs)
      (.add toggle-midi))))
