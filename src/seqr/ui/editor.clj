(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.ui.clip-config :as clip-config])
  (:import
   (javax.swing JTextPane JScrollPane JSplitPane JTable)
   (javax.swing.table AbstractTableModel)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (javax.swing.event TableModelListener TableModelEvent)
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

(defn- text-pane []
  (doto (JTextPane.)
    (.setBackground Color/BLACK)
    (.setForeground Color/GREEN)
    (.setCaretColor Color/WHITE)
    (.setEditable true)
    ;(.setBorder (LineBorder. Color/YELLOW 3))
    (.setFont (Font. "Monospaced" Font/PLAIN 14))))


#_(defn- mk-table-model []
  (proxy [AbstractTableModel] []
    (getColumnCount []
      (:div @cur-clip))
    (getRowCount []
      )))

(defn create-editor [state]
  (let [editor (text-pane)
        _ (reset! cur-editor editor)
        pane (doto (JScrollPane. editor)
               (.setPreferredSize (Dimension. 800 500)))
        config (clip-config/build-table cur-clip pane ["dest1" "dest2"] ["note" "scale"])
        split-pane (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT true config pane)
                     (.setOneTouchExpandable true)
                     (.setDividerSize 10))]
    (add-styles editor)
    split-pane))

(defn set-clip [editor clip]
  (try
    (let [[positions text] (clip/as-str clip :exclude-preamble? true)]
        (.setText editor text)
        (reset! cur-clip {:positions positions :text text :data clip}))
    (catch Exception e
      (prn "Error setting editor content" e))))

(defn highlight-action [editor clip pos]
  (when-some [offsets (get-in @cur-clip [:positions pos])]
    (let [[start end] offsets
          doc (.getStyledDocument editor)
          active-action (.getStyle doc "active-action")
          default (.getStyle doc "editor-default")] 
      (.setCharacterAttributes doc start (- end start) active-action true)
      (future
        (Thread/sleep 300)
        (.setCharacterAttributes doc  start (- end start) default true)))))

(comment  
  (def cl (clip/parse-clip "{:dest few :args {:t 2}} a :2 b"))
  (set-clip @cur-editor cl)
  (highlight-action @cur-editor @cur-clip 4))
