(ns seqr.interpreters
  (:require [seqr.music :as mu])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(defonce interpreters (atom {}))

(defonce midi-interpreters (atom {}))

(defn register-interpreter [key f]
  (swap! interpreters assoc key f))

(defn interpret [{:keys [args interpreter]} action]
  ((get @interpreters interpreter) (merge args action)))

(defn register-midi-interpreter [key f]
  (swap! midi-interpreters assoc key f))

(defn interpret-midi [{:keys [interpreter] :as clip} msg]
  ((get @midi-interpreters interpreter) msg clip))


(defn note [{:keys [action synth] :as data}]
  (let [n (mu/note action)
        freq (mu/midi->hz n)
        args (vec (flatten
                   (into []
                         (assoc
                          (reduce dissoc data [:action :synth :node-id :add-action :target])
                          "freq" freq
                          "note" n))))
        params (merge data {:action action
                            :synth synth
                            :args args})]
    params))

(register-interpreter "note" note)

(defn midi->note
  ([msg]
   (midi->note msg {}))
  ([msg clip]
   (let [gated? (get-in clip [:args :gated])
         cmd (.getCommand msg)
         ret (if (or (= cmd ShortMessage/NOTE_ON)
                     (and gated?
                          (= cmd ShortMessage/NOTE_OFF)))
               {:action (name (mu/find-note-name (.getData1 msg)))}
               {})]
     (if gated?
       (condp = cmd
         ShortMessage/NOTE_ON (assoc ret "gate" 1)
         ShortMessage/NOTE_OFF (assoc ret "gate" 0)
         ret)
       ret))))

(register-midi-interpreter "note" midi->note)
