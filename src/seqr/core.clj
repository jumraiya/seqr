(ns seqr.core
  (:require
   ;[seqr.player :as player]
   [seqr.connections :as conn]
   [seqr.serializers :as se]
   [seqr.samples :as samples]
   [seqr.ui.core :as ui]))

(conn/add-destination! "localhost" 57110 "sc" se/sc-new-synth)

(conn/add-destination! "localhost" 57120 "sc-lang" se/sc-new-synth)

(samples/reset-drum-kits)

(comment
  (ui/create-ui))

