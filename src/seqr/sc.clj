(ns seqr.sc
  (:require [seqr.osc :as osc]
            [seqr.sequencer :as sequencer]
            [seqr.connections :as conn])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(def query-nodes
  (osc/builder "/g_queryTree ?group:0 ?flag:0"))

(def stop-gated
  (osc/builder "/stop_gated 0"))

(def new-group (osc/builder "/g_new ?id"))

(defonce ^:private clip-groups (agent {}))

(defonce clip-group-num (agent -1))

(def eval-sc-code (osc/builder "/eval_code ?code"))

(defn- ensure-clip-group [{:keys [name] :as clip}]
  (let [group-id (or (get @clip-groups name)
                     (send clip-group-num inc))
        ]
    (conn/send! "sc" (new-group {"id" group-id}))))
