(ns seqr.ui.sequencer-controls
  (:require
   [seqr.ui.utils :as utils]
   [seqr.ui.editor :as editor]
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.interpreters :as in]
   [seqr.serializers :as se]
   [seqr.midi :as midi]
   [seqr.sequencer :as sequencer]
   [seqr.connections :as conn])
  (:import
   (javax.swing JButton JLabel JPanel JTextField JComboBox DefaultComboBoxModel)
   (java.awt FlowLayout)
   (java.util.concurrent.locks LockSupport)))

(defonce counter-listener nil)

(defonce midi-input-list (atom nil))

(defonce midi-clock-input-list (atom nil))

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

(defn- play-midi [m]
  (let [cl (editor/get-cur-clip)
        bytes (->> m
                   (in/interpret-midi cl)
                   (in/interpret cl)
                   (se/serialize cl))]
    (when (> (alength bytes) 0)
        (conn/send! (:dest cl) bytes))))

(defn mk-bar []
  (let [play-btn (JButton. "Start")
        bpm-label (JLabel. "BPM:")
        bpm-input (JTextField. (str (sequencer/get-bpm)))
        new-clip-btn (JButton. "New Clip")
        copy-clip-btn (JButton. "Copy Clip")
        midi-label (JLabel. "Input MIDI Device")
        midi-inputs (reset! midi-input-list (doto (JComboBox. (mk-midi-device-list-model))
                                              (.setEditable false)))
        midi-clock-label (JLabel. "Clock MIDI Device")
        midi-clock-inputs (reset! midi-clock-input-list
                                  (doto
                                      (JComboBox.
                                       (mk-midi-device-list-model))
                                    (.setEditable false)))
        reload-midi-devices-btn (JButton. "Refresh")
        toggle-midi (JButton. "Record MIDI")
        toggle-midi-clock (JButton. "Use MIDI Clock")
        mk-clip (JButton. "Make Clip from MIDI")]
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
    (utils/add-action-listener toggle-midi-clock
      (try
        (let [using-clock? (sequencer/using-midi-clock?)
              device-name (.getSelectedItem midi-clock-inputs)]
          (if using-clock?
            (do
              (midi/remove-midi-receiver device-name)
              (sequencer/use-midi-clock false)
              (.setText toggle-midi-clock "Use MIDI Clock"))
            (do
              (midi/set-midi-receiver device-name (sequencer/midi-clock-receiver))
              (sequencer/use-midi-clock true)
              (.setText toggle-midi-clock "Stop receiving clock"))))
        (catch Exception e
          (prn e))))
    (utils/add-action-listener reload-midi-devices-btn
      (.repaint midi-inputs)
      (.repaint midi-clock-inputs))
    (utils/add-action-listener mk-clip
      (editor/set-clip
       (clip/build-from-midi (sequencer/get-bpm) (editor/get-cur-clip))))
    (utils/add-key-action bpm-input "ENTER" "set-bpm"
      (sequencer/set-bpm (Integer/parseInt (.getText bpm-input))))
    (utils/add-action-listener new-clip-btn
      (editor/reset-state))
    (utils/add-action-listener copy-clip-btn
      (editor/set-clip
       (update (editor/get-cur-clip) :name #(str % " copy"))))
    (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
      (.add play-btn)
      (.add bpm-label)
      (.add bpm-input)
      (.add new-clip-btn)
      (.add copy-clip-btn)
      (.add midi-label)
      (.add midi-inputs)
      (.add reload-midi-devices-btn)
      (.add toggle-midi)
      (.add midi-clock-label)
      (.add midi-clock-inputs)
      (.add toggle-midi-clock)
      (.add mk-clip))))

(midi/register-listener :recv-msg :play-midi play-midi)
