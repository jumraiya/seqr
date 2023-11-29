(ns seqr.serializers
  (:require [seqr.osc :as osc]))

(def sc-new-synth
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))
