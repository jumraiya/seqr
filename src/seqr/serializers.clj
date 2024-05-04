(ns seqr.serializers
  (:require [seqr.osc :as osc]
            [seqr.connections :as conn]
            [seqr.music :as mu])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(defonce serializers (atom {}))

(defn register-serializer [key f]
  (swap! serializers assoc key f))

(defn serialize [{:keys [serializer dest]} action]
  (let [f (or (get @serializers serializer)
              (conn/get-serializer dest))]
    (if f
      (if action
        (f action)
        (byte-array []))
      (throw (Exception. "No serializer found")))))

(def sc-new-synth
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(register-serializer "sc" sc-new-synth)

(defn midi-serializer [{:keys [args] :as action}]
  (let [args (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 args))
        msg (ShortMessage.)]
    (cond
      (contains? action "cmd") (.setMessage msg
                                            (get action "cmd")
                                            (or (get action "channel" 1))
                                            (or (get action "data1" 0))
                                            (or (get action "data2" 0)))
      (or (contains? args "note") (contains? args "freq"))
      (let [cmd (if (= 0 (get args "gate"))
                  ShortMessage/NOTE_OFF
                  ShortMessage/NOTE_ON)]
        (.setMessage
         msg
         cmd
         (dec (get action "channel" 1))
         (or (get args "note") (mu/hz->midi (get args "freq")))
         (if (= cmd ShortMessage/NOTE_ON)
           0x64
           0))))
    (.getMessage msg)))

(register-serializer "midi" midi-serializer)

(def reaper-action
  (osc/builder "/action ?action-id"))

(register-serializer "reaper-osc" reaper-action)
