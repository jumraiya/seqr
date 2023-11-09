(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper])
  (:import
   (javax.swing JTextPane JScrollPane)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (java.awt BorderLayout Color Font Dimension)))


(defn rand-color []
  (rand-nth [Color/GREEN Color/ORANGE Color/WHITE
             Color/YELLOW Color/CYAN Color/PINK Color/LIGHT_GRAY]))

(def rand-colors (repeatedly rand-color))

(defn- add-styles [editor]
  (doto (.addStyle editor "active-action" nil)
    (StyleConstants/setBold true)
    (StyleConstants/setBackground (.brighter Color/YELLOW))
    (StyleConstants/setForeground Color/BLACK))
  (doto (.addStyle editor "editor-default" nil)
    (StyleConstants/setBold false)
    (StyleConstants/setBackground Color/BLACK)
    (StyleConstants/setForeground Color/GREEN)))

(defn create-editor [state]
  (let [editor (doto (JTextPane.)
                 (.setBackground Color/BLACK)
                 (.setForeground Color/GREEN)
                 (.setCaretColor Color/WHITE)
                 (.setEditable true)
                 (.setBorder (LineBorder. Color/YELLOW 3))
                 (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        pane (doto (JScrollPane. editor)
               (.setPreferredSize (Dimension. 800 500)))]
    (add-styles editor)
    pane))

(defn set-clip [editor clip]
  (try
    (let [text (clip/as-str clip)]
      (.setText editor text))
    (catch Exception e
      (prn "Error setting editor content" e))))
