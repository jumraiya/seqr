(ns seqr.core
  (:require
   ;[seqr.player :as player]
   [seqr.connections :as conn]
   [seqr.serializers :as se]
   [seqr.samples :as samples]
   [seqr.interpreters :as interp]
   [seqr.ui.core :as ui]
   [seqr.sc :as sc]))

(conn/add-destination! "localhost" 57110 "sc" se/sc-new-synth)

(conn/add-destination! "localhost" 57120 "sc-lang" se/sc-new-synth)

(conn/reset-receiver)

(comment
  (sc/setup)
  (ui/create-ui))
