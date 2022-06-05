(ns seqr.core
  (:require
   [seqr.player :as player]
   [seqr.clip :as cl]
   [seqr.sc :as sc]
   [seqr.connections :as conn]))

(comment
  (conn/connect! "localhost" 57110 :sc)
  (player/ui)
  (player/stop-player)
  (player/is-running?)

  (do
    (player/start-player :bpm 20)
    (player/add-clip
     :test
     (cl/clip
      [a4 :1 c3 | d4 |]
      :args {:synth "sin"}
      :eval "seqr.sc/sc-note"
      :outs {:sc "seqr.sc/s-new"}))
    (player/add-clip
     :test2
     (cl/clip
      [a4 :1 c3 | d4 e2 | f4 :1 d2 |]
      :args {:synth "sin"}
      :eval "seqr.sc/sc-note"
      :group "new"
      :outs {:sc "seqr.sc/s-new"})))

  (player/add-clip
   :test2
   (cl/clip
    [d4 e2 a3]
    :args {:synth "sin"}
    :eval sc/sc-note
    :outs {:sc sc/s-new})))
