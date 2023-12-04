(ns seqr.ui.core
  (:require
   [seqr.ui.editor :as editor]
   [seqr.ui.clip-table :as clip-table]
   [seqr.ui.utils :as utils])
  (:import (javax.swing AbstractAction JComponent JFrame KeyStroke JDialog JTextPane)
           (java.awt BorderLayout Color)
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
      (.put (KeyStroke/getKeyStroke "control LEFT") "focus-editor")
      (.put (KeyStroke/getKeyStroke "ENTER") "load-clip"))
    (doto (.getActionMap clip-table)
      (.put "focus-editor"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (.requestFocusInWindow (.getView (.getViewport clip-pane))))))
      (.put "load-clip"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (let [row (.getSelectedRow (.getSource e))
                      col (.getSelectedColumn (.getSource e))
                      val (.getValueAt (.getSource e) row col)]
                    (when-let [clip (some #(when (= (:name %) val)
                                             %)
                                          (:clips @state))]
                      (editor/set-clip clip)
                      (send state assoc :selected-clip (:name clip))
                      (.requestFocusInWindow (.getView (.getViewport clip-pane))))))))))


(defn show-text-input-dialog [msg value callback]
  (let [input (editor/text-pane)
        dialog (JDialog. @ui-frame msg true)]
    (.setText input value)
    (doto dialog
      (.setDefaultCloseOperation JDialog/DISPOSE_ON_CLOSE)
      (.add input)
      (.setVisible true))))

(defn create-ui []
  (when (and @ui-frame (.isValid @ui-frame))
    (.dispose @ui-frame))
  (let [frame (doto (JFrame. "Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setLayout (BorderLayout.)))
        [editor clip-pane text-view table-view config] (editor/create-editor state)
        _ (utils/add-key-action
           config "control E" "edit-prop"
           (let [r (.getSelectedRow config)
                 c (.getSelectedColumn config)
                 prop (when (odd? c)
                        (.getValueAt (.getModel config) r (dec c)))
                 val (.getValueAt (.getModel config) r c)]
             (when prop 
                (show-text-input-dialog "Edit" val nil))))
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
