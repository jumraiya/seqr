(ns seqr.core
  (:require
   ;[seqr.player :as player]
   [seqr.connections :as conn]
   [seqr.serializers :as se]
   [seqr.sequencer :as sequencer]
   [seqr.samples :as samples]
   [seqr.interpreters :as interp]
   [seqr.ui.core :as ui]
   [seqr.sc :as sc])
  (:import [javax.sound.midi ShortMessage]))

(conn/add-destination! "localhost" 57110 "sc" se/sc-new-synth)

(conn/add-destination! "localhost" 57120 "sc-lang" se/sc-new-synth)

(conn/add-midi-destination! "Reaper" se/midi-serializer)

(conn/add-destination! "localhost" 9090 "reaper-osc")

(sequencer/register-callback
 sequencer/sequencer-paused :stop-midi
 #(let [msg (ShortMessage.)]
    (conn/send! "reaper-osc" (se/reaper-action {"action-id" 40345}))))

(conn/reset-receiver)

(comment
  (sc/setup)
  (ui/create-ui))
