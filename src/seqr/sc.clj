(ns seqr.sc
  (:require [seqr.osc :as osc]
            [seqr.music :as mu]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(defn sc-note [{:keys [action synth] :as data}]
  (let [freq (mu/note->freq action)
        args (vec (flatten
                   (into []
                         (assoc
                          (reduce dissoc data
                                  [:action :synth :node-id :add-action :target]) "freq" freq))))
        params (merge data {:action action
                            :synth synth
                            :args args})]
    params))
