(ns seqr.sc
  (:require [seqr.osc :as osc]
            [seqr.music :as mu]
            [seqr.connections :as c])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(def query-nodes
  (osc/builder "/g_queryTree ?group:0 ?flag:0"))

(def stop-gated
  (osc/builder "/stop_gated 0"))

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

