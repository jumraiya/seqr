(ns seqr.serializable-midi-message
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(gen-class 
 :extends javax.sound.midi.ShortMessage
 :name "com.seqr.MidiMessage"
 :methods [[setData ["[B"] void]])

(defn -setData [this bytes]
  (.setMessage this bytes (alength bytes)))
