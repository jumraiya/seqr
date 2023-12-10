(ns seqr.serializers
  (:require [seqr.osc :as osc]))

(defonce serializers (atom {}))

(defn register-serializer [key f]
  (swap! serializers assoc key f))

(defn serialize [{:keys [serializer]} action]
  ((get @serializers serializer) action))

(def sc-new-synth
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(register-serializer "sc" sc-new-synth)
