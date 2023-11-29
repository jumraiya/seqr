(ns seqr.interpreters
  (:require [seqr.music :as mu]))


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
