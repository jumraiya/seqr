(ns seqr.midi
  (:require [seqr.helper :refer [get-pos calc-period]])
  (:import [javax.sound.midi MidiSystem Receiver MidiMessage ShortMessage]))

(defonce midi-buffer (agent []))

(defonce is-recording? (atom false))

(def record-message
  (proxy [Receiver] []
    (close []
      )
    (send [^MidiMessage m ^Long time-stamp]
      (clojure.core/send midi-buffer conj [time-stamp m]))))


(defn list-devices []
  (doseq [d (MidiSystem/getMidiDeviceInfo)]
    (prn (.toString d)
         "open" (.isOpen (MidiSystem/getMidiDevice d))
         "max transmitters" (.getMaxTransmitters (MidiSystem/getMidiDevice d)))))

(defn get-available-devices []
  (mapv str (MidiSystem/getMidiDeviceInfo)))

(defn find-device [name]
  (some #(let [device (MidiSystem/getMidiDevice %)]
          (when (and (.contains (.toString %) name)
                     (not (> (.getMaxTransmitters device) 0)))
            device))
        (MidiSystem/getMidiDeviceInfo)))


(defn toggle-recording
  [device-name]
  (let [start? (not @is-recording?)]
    (if-let [device (find-device device-name)]
      (do
        (when (not (.isOpen device))
          (.open device))
        (if start?
          (do
            (send midi-buffer (fn [buf] []))
            (.setReceiver (.getTransmitter device) record-message)
            (reset! is-recording? true)
            true)
          (do
            (doseq [t (.getTransmitters device)]
              (.setReceiver t nil))
            (reset! is-recording? false)
            false)))
      (prn (str "Could not find device " device-name)))))

(defn start-recording
  ([]
   (toggle-recording true))
  ([device]
   (toggle-recording true device)))

(defn stop-recording
  ([]
   (toggle-recording false))
  ([device]
   (toggle-recording false device)))

(defn get-quantized-buffer [bpm div]
  "Return a quantized map of populated midi buffer given a microsecond quantum"
  (let [mis-period (* (calc-period bpm div) 1000)]
    (loop [prev (ffirst @midi-buffer) buf (rest @midi-buffer) p 1 m {1 [(-> @midi-buffer first second)]}]
      (if (-> buf empty? not)
        (let [[cur msg] (first buf)
              point (+ p (int (/ (- cur prev) mis-period)))
              m (update m point conj msg)
              buf (rest buf)]
          (if (-> buf empty? not)
            (recur cur buf point m)
            m))
        m))))
