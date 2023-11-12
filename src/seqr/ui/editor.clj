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

(defonce cur-editor (atom nil))

(defonce cur-clip (atom nil))

(defn create-editor [state]
  (let [editor (doto (JTextPane.)
                 (.setBackground Color/BLACK)
                 (.setForeground Color/GREEN)
                 (.setCaretColor Color/WHITE)
                 (.setEditable true)
                 (.setBorder (LineBorder. Color/YELLOW 3))
                 (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        _ (reset! cur-editor editor)
        pane (doto (JScrollPane. editor)
               (.setPreferredSize (Dimension. 800 500)))]
    (add-styles editor)
    pane))

(defn set-clip [editor clip]
  (try
    (let [[positions text] (clip/as-str clip)]
      (.setText editor text)
      (reset! cur-clip {:pos (into []
                                   (comp (map second) (mapcat vals))
                                   positions)
                        :clip clip}))
    (catch Exception e
      (prn "Error setting editor content" e))))

(defn highlight-action [editor clip point]
  (when (<= point (count (:pos clip)))
    (let [[start end] pos
          doc (.getStyledDocument editor)
          active-action (.getStyle doc "active-action")
          default (.getStyle doc "editor-default")] 
      (.setCharacterAttributes doc start (- end start) active-action true)
      (future
        (Thread/sleep 300)
        (.setCharacterAttributes doc  start (- end start) default true)))))

(comment
  
  (def cl (clip/parse-clip "a :2 b"))

  (set-clip @cur-editor cl)
  (highlight-action @cur-editor @cur-clip 4))
