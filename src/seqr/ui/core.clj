(ns seqr.ui.core
  (:require
   [seqr.ui.editor :as editor])
  (:import (javax.swing JFrame)
           (java.awt BorderLayout)
           (com.formdev.flatlaf FlatDarkLaf)))

(FlatDarkLaf/setup)

(defonce ^:private ui-frame (atom nil))

(defonce ^:private state
  (atom {:selected [0 0]
         :clips {}
         :editing-clip nil
         :clip-selected {}
         :group-selected 0}))

(defn create-ui []
  (when (and @ui-frame (.isValid @ui-frame))
    (.dispose @ui-frame))
  (let [frame (doto (JFrame. "Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setLayout (BorderLayout.)))
        content (doto (.getContentPane frame)
                  (.add (editor/create-editor state) BorderLayout/WEST))]
    (doto frame
      (.pack)
      (.setVisible true))
    (reset! ui-frame frame)))

(comment
  (create-ui))
