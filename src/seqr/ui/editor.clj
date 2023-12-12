(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.connections :as conn]
   [seqr.helper :as helper]
   [seqr.ui.clip-config :as clip-config]
   [seqr.ui.utils :as utils]
   [seqr.sequencer :as sequencer])
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

(defn set-clip [{:keys [div point] :as clip}]
  (try
    (when (and div point)
        (let [[positions text] (clip/as-str clip :exclude-preamble? true) ;(clip/as-str clip)
              ]
          (.setText @clip-text-editor text
                    #_(if-let [pos (get-in positions [1 1])]
                      (.substring text (first pos))
                      ""))
          (send cur-clip merge {:positions positions :text text :data clip})
          (.fireTableStructureChanged (.getModel @clip-table-editor))
          (.fireTableStructureChanged (-> @clip-config-editor .getViewport .getView .getModel))))
    (catch Exception e
      (prn "Error setting editor content" e))))


(defn- save-clip [ui-state {:keys [interpreter serializer dest point name div] :as clip}]
  (let [clip-pos (or (some #(when (= (:name (second %)) name)
                              (first %))
                           (map-indexed vector (:clips @ui-state)))
                     (count (:clips @ui-state)))]
    (when name
      (send ui-state assoc-in [:clips clip-pos] clip))
    (when (and point div)
      (let [[positions text] (clip/as-str clip :exclude-preamble? true) ;(clip/as-str clip)
            ]
        (.setText @clip-text-editor
                  text
                  #_(if-let [pos (get-in positions [1 1])]
                    (.substring text (first pos))
                    ""))
        (send cur-clip merge {:positions positions :text text})
        (when (and name
                   (not (empty? interpreter))
                   (or serializer (conn/get-serializer dest)))
          (sequencer/save-clip clip))))
    (send cur-clip assoc :data clip)
    (.fireTableStructureChanged (.getModel @clip-table-editor))))

(defn- mk-table-model [ui-state]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      (or (-> @cur-clip :data :div) 4))
    (getRowCount []
      (int (/ sequencer/MAX-CLIP-ACTIONS (-> @cur-clip :data (:div 4))))
      #_(if (and (contains? (:data @cur-clip) :point)
                 (contains? (:data @cur-clip) :div))
          (int (/ (-> @cur-clip :data :point) (-> @cur-clip :data :div)))
          1))
    (getValueAt [row col]
      (let [[start end] (get-in @cur-clip [:positions (inc row) (inc col)])]
        (when start
          (.substring (:text @cur-clip) start end))))
    (setValueAt [val row col]
      (when-let [div (-> @cur-clip :data :div)]
        (let [actions (get-in (clip/parse-clip val (:data @cur-clip))
                              [1 1])
              new-clip (assoc-in (:data @cur-clip) [(inc row) (inc col)] actions)
              at-point (helper/get-point (inc row) (inc col) div)]
          (save-clip ui-state
                     (if (and (seq actions)
                              (< (:point new-clip 1) (inc at-point)))
                       (assoc new-clip :point (inc at-point))
                       new-clip)))))
    (isCellEditable [row col]
      true)))

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
    (let [config (clip-config/get-config-map
                  (-> @clip-config-editor (.getViewport) (.getView)))]
      (when (:div config)
          (save-clip ui-state (clip/parse-clip
                               (.getText text)
                               config)))))

  #_(doseq [c [table config-table]]
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
                       (.setModel (mk-table-model state))
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
                cur-clip state save-clip pane ["sc"] ["note" "scale"])
        _ (reset! clip-config-editor config)
        _ (add-keybindings state pane text-editor table-editor (.getView (.getViewport config)))
        split-pane (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT true config pane)
                     (.setOneTouchExpandable true)
                     (.setDividerSize 10))]
    (add-styles text-editor)
    (when (some? (:data @cur-clip))
      (set-clip (:data @cur-clip)))
    [split-pane pane text-editor table-editor (.getView (.getViewport config))]))

(defn highlight-action [counter]
  (let [pos (helper/get-pos
             counter
             (-> @cur-clip :data :div)
             {:player-div (:div @sequencer/state)
              :size (dec (-> @cur-clip :data :point))})]
    (when-let [offsets (seq (get-in (:positions @cur-clip) pos))]
      (let [[start end] offsets
            doc (.getStyledDocument @clip-text-editor)
            active-action (.getStyle doc "active-action")
            default (.getStyle doc "editor-default")] 
        (.setCharacterAttributes doc start (- end start) active-action true)
        (future
          (Thread/sleep 300)
          (.setCharacterAttributes doc  start (- end start) default true))))))

(comment  
  (def cl (clip/parse-clip "{:args {t 2 atk 0.01 a 2 b 3 g 3 r 12}} a :2 b {t 1}"))
  (set-clip cl)
  (highlight-action @clip-text-editor @cur-clip 4))
