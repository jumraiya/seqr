(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.ui.clip-config :as clip-config])
  (:import
   (javax.swing AbstractAction JComponent JTextPane JScrollPane JSplitPane JTable KeyStroke JLabel)
   (javax.swing.table AbstractTableModel TableCellRenderer)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (javax.swing.event TableModelListener TableModelEvent)
   (java.awt BorderLayout Color Font Dimension)
   (java.awt.event FocusListener)))

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

(defonce ^:private clip-text-editor (atom nil))

(defonce ^:private clip-table-editor (atom nil))

(defonce ^:private cur-clip (atom nil))

(defonce ^:private clip-editor (atom nil))

(def focus-listener
  (reify FocusListener
    (focusGained [this e]
      (when-let [scroll-pane (.getParent (.getParent (.getSource e)))]
          (.setBorder scroll-pane (LineBorder. Color/YELLOW 2))))
    (focusLost [this e]
      (try
        (when-let [scroll-pane (cond-> e
                                 some? (.getSource)
                                 some? (.getParent)
                                 some? (.getParent))]
          (.setBorder scroll-pane nil))
        (catch Exception e)))))

(defn- text-pane []
  (doto (JTextPane.)
    (.setBackground Color/BLACK)
    (.setForeground Color/GREEN)
    (.setCaretColor Color/WHITE)
    (.setEditable true)
    (.addFocusListener focus-listener)
    (.setFont (Font. "Monospaced" Font/PLAIN 14))))

(defn set-clip [clip]
  (try
    (let [[positions text] (clip/as-str clip)]
      (.setText @clip-text-editor
                (if-let [pos (get-in positions [1 1])]
                  (.substring text (first pos))
                  ""))
      (reset! cur-clip {:positions positions :text text :data clip})
      (.fireTableStructureChanged (.getModel @clip-table-editor)))
    (catch Exception e
      (prn "Error setting editor content" e))))

(defn- mk-table-model []
  (proxy [AbstractTableModel] []
    (getColumnCount []
      (or (-> @cur-clip :data :div) 0))
    (getRowCount []
      (->> @cur-clip :data keys (filter number?) (into [0]) (apply max)))
    (getValueAt [row col]
      (let [[start end] (get-in @cur-clip [:positions (inc row) (inc col)])]
        (when start
          (.substring (:text @cur-clip) start end))))
    (setValueAt [val row col]
      (let [actions (get-in (clip/parse-clip val {:args (-> @cur-clip :data :args)})
                            [1 1])]
        (set-clip (assoc-in (:data @cur-clip) [(inc row) (inc col)] actions))))
    (isCellEditable [row col]
      true)))

(defn- save-clip [ui-state]
  (let [clip-pos (or (some #(when (= (:name (second %)) (-> @cur-clip :data :name))
                              (first %))
                           (map-indexed vector (:clips @ui-state)))
                     (max 0 (count (:clips @ui-state))))]
    (send ui-state assoc-in [:clips clip-pos] (:data @cur-clip))))

(defn- add-keybindings [ui-state editor text table config-table]
  (doto (.getInputMap config-table)
    (.put (KeyStroke/getKeyStroke "control DOWN") "focus-editor"))
  (doto (.getInputMap text)
    (.put (KeyStroke/getKeyStroke "control T") "toggle-table-mode"))
  (doto (.getInputMap table JComponent/WHEN_IN_FOCUSED_WINDOW)
    (.put (KeyStroke/getKeyStroke "control T") "toggle-table-mode"))
  (doto (.getActionMap config-table)
    (.put "focus-editor"
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (.requestFocusInWindow (.getView (.getViewport editor)))))))
  (doto (.getActionMap text)
    (.put "toggle-table-mode"
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (.setViewportView editor table)
              (.requestFocusInWindow table)))))
  (doto (.getActionMap table)
    (.put "toggle-table-mode"
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (.setViewportView editor text)
              (.requestFocusInWindow text)))))
  (doseq [c [text table]]
    (doto (.getInputMap c)
      (.put (KeyStroke/getKeyStroke "control UP") "focus-config"))
    (doto (.getActionMap c)
      (.put "focus-config"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (.requestFocusInWindow config-table))))))
  (doseq [c [text table config-table]]
    (doto (.getInputMap c)
      (.put (KeyStroke/getKeyStroke "control S") "save-clip"))
    (doto (.getActionMap c)
      (.put "save-clip"
            (proxy [AbstractAction] []
              (actionPerformed [e]
                (save-clip ui-state)))))))

(defn- mk-table-editor []
  (proxy [JTable] []
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [f (JLabel. value)]
            (when hasFocus
              (.setBorder f (LineBorder. Color/YELLOW 2)))            
            f))))))

(defn create-editor [state]
  (let [text-editor (text-pane)
        _ (reset! clip-text-editor text-editor)
        table-editor (doto (mk-table-editor)
                       (.setModel (mk-table-model))
                       (.setTableHeader nil)
                       (.addFocusListener focus-listener)
                       (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        _ (reset! clip-table-editor table-editor)
        pane (doto (JScrollPane. text-editor)
               (.setPreferredSize (Dimension. 800 500)))
        _ (reset! clip-editor pane)
        config (clip-config/build-table
                cur-clip pane ["dest1" "dest2"] ["note" "scale"])
        _ (add-keybindings state pane text-editor table-editor (.getView (.getViewport config)))
        split-pane (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT true config pane)
                     (.setOneTouchExpandable true)
                     (.setDividerSize 10))]
    (add-styles text-editor)
    [split-pane pane text-editor table-editor (.getView (.getViewport config))]))

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
  (def cl (clip/parse-clip "{:args {t 2 atk 0.01 a 2 b 3 g 3 r 12}} a :2 b {t 1}"))
  (set-clip cl)
  (highlight-action @clip-text-editor @cur-clip 4))
