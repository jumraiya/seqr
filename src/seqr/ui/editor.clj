(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.ui.clip-config :as clip-config]
   [seqr.ui.utils :as utils])
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

(defonce ^:private clip-config-editor (atom nil))

(defonce ^:private cur-clip (agent nil))

(defonce ^:private clip-editor (atom nil))

(defonce ^:private name-counter (atom 0))

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

(defn set-clip [clip]
  (try
    (let [[positions text] (clip/as-str clip)]
      (.setText @clip-text-editor
                (if-let [pos (get-in positions [1 1])]
                  (.substring text (first pos))
                  ""))
      (send cur-clip merge {:positions positions :text text :data clip})
      (.fireTableStructureChanged (.getModel @clip-table-editor))
      (.fireTableStructureChanged (-> @clip-config-editor .getViewport .getView .getModel)))
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
  (let [clip-name (-> @cur-clip :data :name)
        clip-name (if (not (clojure.string/blank? clip-name))
                    clip-name
                    (str "clip-" (swap! name-counter inc)))
        clip-pos (or (some #(when (= (:name (second %)) clip-name)
                              (first %))
                           (map-indexed vector (:clips @ui-state)))
                     (count (:clips @ui-state)))]
    (when (not= clip-name (-> @cur-clip :data :name))
      (send cur-clip assoc-in [:data :name] clip-name))
    (send ui-state assoc-in [:clips clip-pos] (:data @cur-clip))))

(defn- add-keybindings [ui-state editor text table config-table]
  (utils/add-key-action config-table "control DOWN" "focus-editor"
    (.requestFocusInWindow (.getView (.getViewport editor))))

  (doseq [c [text table]]
    (utils/add-key-action c "control UP" "focus-config"
      (.requestFocusInWindow config-table)))

  (utils/add-key-action text "control T" "toggle-table-mode"
    (.setViewportView editor table)
    (.requestFocusInWindow table))

  (utils/add-key-action-with-focus
      table "control T" "toggle-table-mode" JComponent/WHEN_IN_FOCUSED_WINDOW
    (.setViewportView editor text)
    (.requestFocusInWindow text))

  (utils/add-key-action text "control S" "save-clip"
    (send cur-clip
          assoc :data (clip/parse-clip
                       (.getText text)
                       (reduce #(if (number? %2) (dissoc %1 %2) %1)
                               (:data @cur-clip) (keys (:data @cur-clip)))))
    (save-clip ui-state))
  
  (doseq [c [table config-table]]
    (utils/add-key-action c "control S" "save-clip"
      (save-clip ui-state))))

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
  (let [text-editor (doto (utils/text-pane)
                      (.addFocusListener focus-listener))
        _ (reset! clip-text-editor text-editor)
        table-editor (doto (mk-table-editor)
                       (.setModel (mk-table-model))
                       (.setTableHeader nil)
                       (.addFocusListener focus-listener)
                       (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        _ (utils/add-key-action
           table-editor "control E" "edit-action"
           (let [r (.getSelectedRow table-editor)
                 c (.getSelectedColumn table-editor)
                 val (.getValueAt (.getModel table-editor) r c)]
             (utils/show-text-input-dialog
              (.getTopLevelAncestor (.getSource e)) "Edit" val #(.setValueAt (.getModel table-editor) % r c))))
        _ (reset! clip-table-editor table-editor)
        pane (doto (JScrollPane. text-editor)
               (.setPreferredSize (Dimension. 800 500)))
        _ (reset! clip-editor pane)
        config (clip-config/build-table
                cur-clip pane ["dest1" "dest2"] ["note" "scale"])
        _ (reset! clip-config-editor config)
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
