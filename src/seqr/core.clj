(ns seqr.core
  (:require
   [seqr.player :as player]
   [seqr.clip :as cl]
   [seqr.sc :as sc]
   [seqr.connections :as conn]
   [seqr.midi :as midi]
   [seqr.serializers :as se]))

(conn/add-destination! "localhost" 57110 "sc" se/sc-new-synth)

(conn/add-destination! "localhost" 57120 "sc-lang")

(comment
  (player/ui)
  (player/stop-player)
  (player/is-running?)

  (midi/start-recording)
  (midi/stop-recording)
  (do
    (player/start-player :bpm 80)

    (player/add-clip
     "test"
     (cl/parse-clip
      "{:args {:synth bpfsaw atk 0.01}
      :eval seqr.sc/note
      :div 4
      :outs {:sc seqr.sc/s-new}}
      a4 :1 c3 | d4 |"))

    (player/add-clip
     "test2"
     (cl/parse-clip
      "{:args {:synth bass}
      :eval seqr.sc/note
      :div 6
      :outs {:sc seqr.sc/s-new}}
      a2 c2 | d2 |"))
    (player/rm-clip "test")
    )

  (player/add-clip
   :test2
   (cl/clip
       [d4 e2 a3]
       :args {:synth "sin"}
       :eval sc/sc-note
       :outs {:sc sc/s-new})))
