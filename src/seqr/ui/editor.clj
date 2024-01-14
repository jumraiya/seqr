(ns seqr.ui.editor
  (:require
   [seqr.clip :as clip]
   [seqr.connections :as conn]
   [seqr.interpreters :as inter]
   [seqr.helper :as helper]
   [seqr.ui.clip-config :as clip-config]
   [seqr.ui.tracker :as tracker]
   [seqr.ui.utils :as utils]
   [seqr.sequencer :as sequencer]
   [seqr.music :as m])
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

(defonce ^:private tracker-view (atom nil))

(defonce ^:private cur-clip (agent nil))

(defonce ^:private clip-editor (atom nil))

(defonce ^:private name-counter (atom 0))

(defonce ^:private active-cell (atom nil))

(declare set-clip)

(defn get-cur-clip []
  (:data @cur-clip))

(defn reset-state []
  (set-clip {:div 4 :point 5 :name "new-clip"}))

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
        (let [[positions text] (clip/as-str clip :exclude-preamble? true)]
          (.setText @clip-text-editor text)
          (send cur-clip merge {:positions positions :text text :data clip})
          (.fireTableStructureChanged (.getModel @clip-table-editor))
          (.fireTableStructureChanged (-> @clip-config-editor .getViewport .getView .getModel))))
    (catch Exception e
      (prn "Error setting editor content" e))))

(defn save-clip [ui-state {:keys [interpreter serializer dest point name div] :as clip}]
  (let [clip-pos (or (some #(when (= (:name (second %)) name)
                              (first %))
                           (map-indexed vector (:clips @ui-state)))
                     (count (:clips @ui-state)))]
    (when name
      (send ui-state assoc-in [:clips clip-pos] clip))
    (when (and point div)
      (let [[positions text] (clip/as-str clip :exclude-preamble? true)]
        (.setText @clip-text-editor text)
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
      (int (/ sequencer/MAX-CLIP-ACTIONS (-> @cur-clip :data (:div 4)))))
    (getValueAt [row col]
      (let [[start end] (get-in @cur-clip [:positions (inc row) (inc col)])]
        (when start
          (.substring (:text @cur-clip) start end))))
    (setValueAt [val row col]
      (when-let [div (-> @cur-clip :data :div)]
        (let [actions (clip/parse-actions val (:data @cur-clip))
              new-clip (assoc-in (:data @cur-clip) [(inc row) (inc col)] actions)
              at-point (helper/get-point (inc row) (inc col) div)]
          (save-clip ui-state
                     (if (and (seq actions)
                              (< (:point new-clip 1) (inc at-point)))
                       (assoc new-clip :point (inc at-point))
                       new-clip)))))
    (isCellEditable [row col]
      true)))

 (defn- shift-clip [point ui-state direction]
  (let [new-clip  (if (= :left direction)
                    (clip/shift-left (:data @cur-clip) point)
                    (clip/shift-right (:data @cur-clip) point))]
    (save-clip ui-state new-clip)))

(defn- find-text-point [text]
  (let [[bar note] (some
                      (fn [[bar locs]]
                        (some
                         #(when (and (>= (.getCaretPosition text)
                                         (-> % second first))
                                     (<= (.getCaretPosition text)
                                         (-> % second second)))
                            [bar (first %)])
                         locs))
                      (:positions @cur-clip))]
    (when (and bar note)
      (helper/get-point bar note (-> @cur-clip :data :div)))))

(defn- find-table-point [table-editor]
  (when (-> table-editor
            (.getModel)
            (.getValueAt (.getSelectedRow table-editor)
                         (.getSelectedColumn table-editor))
            empty? not)
    (helper/get-point (inc (.getSelectedRow table-editor))
                      (inc (.getSelectedColumn table-editor))
                      (-> @cur-clip :data :div))))

(defn- find-tracker-point [tracker]
  (let [point (helper/get-wrapped-point
               (inc (.getSelectedRow tracker))
               (-> @cur-clip :data :div)
               (dec (-> @cur-clip :data :point))
               (sequencer/get-div))
        [b n] (helper/get-pos point (-> @cur-clip :data :div))]
    (when (seq (get-in @cur-clip [:data b n]))
      point)))


 (defn- move-tracker-selection [state table dir]
   (let [row (.getSelectedRow table)
         col (.getSelectedColumn table)]
     (if (= col 0)
       (.changeSelection
        table
        (case dir
          :up (max 0 (dec row))
          :down (min (dec (.getRowCount (.getModel table))) (inc row)))
        col false false)
       (loop [r (case dir :up (dec row) :down (inc row))]
         (if (<= r 0)
           (.changeSelection table 0 col false false)
           (if
            (>= r (.getRowCount (.getModel table)))
             (.changeSelection table (dec (.getRowCount (.getModel table))) col false false)
             (if
              (int?
               (when-let [{:keys [div point] :as cl}
                          (get (:clips @state) (dec col))]
                 (when-let [seq-div (sequencer/get-div)]
                   (helper/get-wrapped-point (inc r) div (dec point) seq-div))))
               (.changeSelection table r col false false)
               (recur (case dir :up (dec r) :down (inc r))))))))))

(defn- add-keybindings [ui-state editor text table tracker config-table]
  (utils/add-key-action config-table "control DOWN" "focus-editor"
                        (.requestFocusInWindow (.getView (.getViewport editor))))

  (doseq [c [text table tracker]]
    (utils/add-key-action c "control UP" "focus-config"
                          (.requestFocusInWindow config-table)))

  (utils/add-key-action
      table "control E" "edit-action"
    (let [r (.getSelectedRow table)
          c (.getSelectedColumn table)
          val (.getValueAt (.getModel table) r c)]
      (utils/show-action-editor
       (.getTopLevelAncestor (.getSource e))
       (:data @cur-clip)
       (find-table-point table)
       #(.setValueAt (.getModel table) % r c))))

  (utils/add-key-action
      text "control E" "edit-action"
    (when-let [point (find-text-point text)]
      (let [pos (helper/get-pos point (-> @cur-clip :data :div))]
          (utils/show-action-editor
           (.getTopLevelAncestor (.getSource e))
           (:data @cur-clip)
           point
           #(let [actions (clip/parse-actions % (:data @cur-clip))]
              (save-clip ui-state (assoc-in (:data @cur-clip) pos actions)))))))
  
  (utils/add-key-action text "control T" "toggle-table-mode"
    (.setViewportView editor table)
    (.requestFocusInWindow table)
    (send ui-state assoc ::cur-view :table))

  (utils/add-key-action text "control L" "toggle-tracker-mode"
                        (.setViewportView editor tracker)
                        (.requestFocusInWindow tracker))

  (utils/add-key-action tracker "control L" "toggle-tracker-mode"
                        (case (::cur-view @ui-state :text)
                          :text (do
                                  (.setViewportView editor text)
                                  (.requestFocusInWindow text))
                          :table (do
                                   (.setViewportView editor table)
                                   (.requestFocusInWindow table))))

  (utils/add-key-action-with-focus
   table "control T" "toggle-table-mode" JComponent/WHEN_IN_FOCUSED_WINDOW
   (.setViewportView editor text)
   (.requestFocusInWindow text)
   (send ui-state assoc ::cur-view :text))

  (utils/add-key-action-with-focus
   table "control L" "toggle-tracker-mode" JComponent/WHEN_IN_FOCUSED_WINDOW
   (.setViewportView editor tracker)
   (.requestFocusInWindow tracker))

  (utils/add-key-action text "control S" "save-clip"
                        (let [config (clip-config/get-config-map
                                      (-> @clip-config-editor (.getViewport) (.getView)))]
                          (when (:div config)
                            (save-clip ui-state (clip/parse-clip
                                                 (.getText text)
                                                 config)))))

  (utils/add-key-action text "shift control LEFT" "shift-left"
                        (when-let [point (find-text-point text)]
                          (shift-clip point ui-state :left)))

  (utils/add-key-action text "shift control RIGHT" "shift-right"
                        (when-let [point (find-text-point text)]
                          (shift-clip point ui-state :right)))

  (utils/add-key-action table "shift control LEFT" "shift-left"
                        (when-let [point (find-table-point table)]
                          (shift-clip point ui-state :left)))

  (utils/add-key-action table "shift control RIGHT" "shift-right"
                        (when-let [point (find-table-point table)]
                          (shift-clip point ui-state :right)))

  (utils/add-key-action tracker "shift control DOWN" "shift-right"
                        (when-let [point (find-tracker-point tracker)]
                          (shift-clip point ui-state :right)
                          (.fireTableDataChanged (.getModel tracker))))

  (utils/add-key-action tracker "shift control UP" "shift-left"
                        (when-let [point (find-tracker-point tracker)]
                          (shift-clip point ui-state :left)
                          (.fireTableDataChanged (.getModel tracker))))

  (utils/add-key-action tracker "UP" "move-up"
                        (move-tracker-selection ui-state tracker :up))

  (utils/add-key-action tracker "DOWN" "move-down"
                        (move-tracker-selection ui-state tracker :down))

  (utils/add-key-action tracker "control SPACE" "set-play-window"
    (let [rows (.getSelectedRows tracker)]
      (sequencer/set-play-window
       (inc (apply min rows)) (inc (apply max rows)))
      (.fireTableDataChanged (.getModel tracker))))

  (utils/add-key-action tracker "shift SPACE" "reset-play-window"
    (sequencer/reset-play-window)
    (.fireTableDataChanged (.getModel tracker))))

(defn- mk-table-editor []
  (proxy [JTable] []
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [f (JLabel. value)]
            (when (and (not (empty? value))
                       (= (dec (or (first @active-cell) 0)) row)
                       (= (dec (or (second @active-cell) 0)) col))
              (doto f
                (.setBackground (.brighter Color/YELLOW))
                (.setForeground Color/BLACK)
                (.setOpaque true)))
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
        _ (reset! clip-table-editor table-editor)
        tracker (tracker/build state set-clip save-clip)
        _ (reset! tracker-view tracker)
        pane (doto (JScrollPane. text-editor)
               (.setPreferredSize (Dimension. 800 500)))
        _ (reset! clip-editor pane)
        config (clip-config/build-table
                cur-clip state save-clip pane
                (conn/get-destinations) (inter/get-interpreters))
        _ (reset! clip-config-editor config)
        _ (add-keybindings state pane text-editor table-editor tracker (.getView (.getViewport config)))
        split-pane (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT true config pane)
                     (.setOneTouchExpandable true)
                     (.setDividerSize 10))]
    (add-styles text-editor)
    (sequencer/register-callback
     sequencer/sequencer-paused :reset-highlight
     #(cond
        (= (.getView (.getViewport @clip-editor)) @tracker-view)
        (do
          (reset! tracker/active-row nil)
          (.fireTableDataChanged (.getModel @tracker-view)))
        (= (.getView (.getViewport @clip-editor)) @clip-table-editor)
        (do
          (reset! active-cell nil)
          (.fireTableDataChanged (.getModel @clip-table-editor)))
        :else
        (let [doc (.getStyledDocument @clip-text-editor)
              default (.getStyle doc "editor-default")]
          (.setCharacterAttributes doc 0 (.length ^String (.getText @clip-text-editor)) default true))))
    (when (some? (:data @cur-clip))
      (set-clip (:data @cur-clip)))
    [split-pane pane text-editor table-editor (.getView (.getViewport config)) tracker]))

 (defn highlight-action [counter]
  (if (= (.getView (.getViewport @clip-editor)) @tracker-view)
    (let [scroll-pos (* counter (.getRowHeight @tracker-view))
          model (.getModel (.getVerticalScrollBar @clip-editor))]
      (reset! tracker/active-row (dec counter))
      (-> @tracker-view
          (.getModel)
          (.fireTableRowsUpdated (dec counter) (dec counter)))
      (future
        (Thread/sleep 10)
        (-> @tracker-view
            (.getModel)
            (.fireTableRowsUpdated (max 0 (- counter 2)) (dec counter))))
      (when (or (< scroll-pos (.getValue model))
                (> scroll-pos (+ (.getValue model) (.getExtent model))))
        (.setValue (.getVerticalScrollBar @clip-editor) scroll-pos)
        (.fireTableDataChanged (.getModel @tracker-view))))
    (let [{:keys [point div]} (:data @cur-clip)
          player-div (:div @sequencer/state)
          counter (helper/get-wrapped-point counter div (dec point) player-div)]
      (when-let [pos (and counter
                          (helper/get-pos
                           counter div))]
        (if (= (.getView (.getViewport @clip-editor)) @clip-table-editor)
          (when (first pos)
            (reset! active-cell pos)
            (.fireTableCellUpdated (.getModel @clip-table-editor)
                                   (dec (first pos))
                                   (dec (second pos)))
            (future
              (Thread/sleep 300)
              (.fireTableCellUpdated (.getModel @clip-table-editor)
                                     (dec (first pos))
                                     (dec (second pos)))))
          (when-let [offsets (seq (get-in (:positions @cur-clip) pos))]
            (let [[start end] offsets
                  doc (.getStyledDocument @clip-text-editor)
                  active-action (.getStyle doc "active-action")
                  default (.getStyle doc "editor-default")]
              (.setCharacterAttributes doc start (- end start) active-action true)
              (future
                (Thread/sleep 300)
                (.setCharacterAttributes doc  start (- end start) default true)))))))))

(comment  
  (def cl (clip/parse-clip "{:args {t 2 atk 0.01 a 2 b 3 g 3 r 12}} a :2 b {t 1}"))
  (set-clip cl)
  (highlight-action @clip-text-editor @cur-clip 4))
