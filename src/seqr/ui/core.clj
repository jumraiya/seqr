(ns seqr.ui.core
  (:require
   [seqr.ui.editor :as editor]
   [seqr.ui.clip-table :as clip-table]
   [seqr.ui.utils :as utils]
   [seqr.ui.menu-bar :as menu-bar]
   [seqr.ui.sequencer-controls :as controls])
  (:import (javax.swing AbstractAction JComponent JFrame KeyStroke JTextPane SwingUtilities JPanel BoxLayout)
           (java.awt BorderLayout Color Dimension Point)
           (com.formdev.flatlaf FlatDarkLaf)))

(FlatDarkLaf/setup)

(defonce ^:private ui-frame (atom nil))

(def default-state
  {:clips []
   :selected-clip nil
   :selected-clips #{}
   :clips-marked-to-be-active #{}
   :clips-marked-to-be-inactive #{}})

(defonce ^:private state
  (agent default-state))

(defn reset-state []
  (send state (constantly default-state)))

(defn- add-key-bindings [clip-pane [text-view table-view config tracker :as components] clip-table clip-config]
  (doseq [c components]
    (utils/add-key-action c "control RIGHT" "focus-clip-table"
      (.requestFocusInWindow clip-table)))

  (utils/add-key-action clip-table "control LEFT" "focus-editor"
    (.requestFocusInWindow (.getView (.getViewport clip-pane))))

  (utils/add-key-action clip-table "ENTER" "load-clip"
    (let [row (.getSelectedRow (.getSource e))
          col (.getSelectedColumn (.getSource e))
          val (.getValueAt (.getSource e) row col)]
      (when-let [clip (some #(when (= (:name %) val)
                               %)
                            (:clips @state))]
        (editor/set-clip clip)
        (send state assoc :selected-clip (:name clip))
        (.requestFocusInWindow (.getView (.getViewport clip-pane)))
        (.setViewportView clip-pane text-view)
        (.requestFocusInWindow text-view)))))


(defn create-ui []
  (when (and @ui-frame (.isValid @ui-frame))
    (.dispose @ui-frame))
  (let [frame (doto (JFrame. "Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setLayout (BorderLayout.)))
        [editor clip-pane text-view table-view config tracker] (editor/create-editor state)
        clip-table (clip-table/create state)
        _ (add-key-bindings
           clip-pane [text-view table-view config tracker]
           (-> clip-table (.getViewport) (.getView)) config)
        menus (menu-bar/build state reset-state)
        controls (controls/mk-bar)
        top-bar (JPanel.)
        content (doto (.getContentPane frame)
                  (.add (doto top-bar
                          (.setAlignmentX JPanel/LEFT_ALIGNMENT)
                          (.setLayout (BoxLayout. top-bar BoxLayout/Y_AXIS))
                          (.add menus)
                          (.add controls))
                        BorderLayout/NORTH)
                  (.add editor BorderLayout/WEST)
                  (.add clip-table BorderLayout/EAST))]
    (doto frame
      (.pack)
      (.setVisible true))
    (reset! ui-frame frame)))

(comment
  (create-ui))
