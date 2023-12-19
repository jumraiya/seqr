(ns seqr.serializers
  (:require [seqr.osc :as osc]
            [seqr.connections :as conn]))

(defonce serializers (atom {}))

(defn register-serializer [key f]
  (swap! serializers assoc key f))
#trace
(defn serialize [{:keys [serializer dest]} action]
  (let [f (or (get @serializers serializer)
              (conn/get-serializer dest))]
    (if f
        (f action)
        (throw (Exception. "No serializer found")))))

(def sc-new-synth
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(register-serializer "sc" sc-new-synth)

