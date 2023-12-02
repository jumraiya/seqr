(ns seqr.ui.core
  (:require
   [seqr.ui.editor :as editor]
   [seqr.ui.clip-table :as clip-table])
  (:import (javax.swing AbstractAction JComponent JFrame KeyStroke)
           (java.awt BorderLayout)
           (com.formdev.flatlaf FlatDarkLaf)))

(FlatDarkLaf/setup)

(defonce ^:private ui-frame (atom nil))

(defonce ^:private state
  (agent {:clips [] :selected-clip nil}))

(defn- add-key-bindings [clip-pane components clip-table]
  (doseq [c components]
    (doto (.getInputMap c)
      (.put (KeyStroke/getKeyStroke "control RIGHT") "focus-clip-table"))
    (doto (.getActionMap c)
      (.put "focus-clip-table"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (.requestFocusInWindow clip-table))))))
  (doto (.getInputMap clip-table)
      (.put (KeyStroke/getKeyStroke "control LEFT") "focus-editor"))
    (doto (.getActionMap clip-table)
      (.put "focus-editor"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (.requestFocusInWindow (.getView (.getViewport clip-pane))))))))

(defn create-ui []
  (when (and @ui-frame (.isValid @ui-frame))
    (.dispose @ui-frame))
  (let [frame (doto (JFrame. "Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setLayout (BorderLayout.)))
        [editor clip-pane text-view table-view config] (editor/create-editor state)
        clip-table (clip-table/create state)
        _ (add-key-bindings
           clip-pane [text-view table-view config] (-> clip-table (.getViewport) (.getView)))
        content (doto (.getContentPane frame)
                  (.add editor BorderLayout/WEST)
                  (.add clip-table BorderLayout/EAST))]
    (doto frame
      (.pack)
      (.setVisible true))
    (reset! ui-frame frame)))

(comment
  (create-ui))
