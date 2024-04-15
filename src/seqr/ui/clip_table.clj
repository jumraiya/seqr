(ns seqr.ui.clip-table
  (:require
   [seqr.ui.utils :as utils]
   [seqr.sequencer :as sequencer]
   [seqr.sc :as s.sc])
  (:import
   (java.awt.event ComponentListener ComponentEvent)
   (javax.swing JComboBox JTextPane JScrollPane JSplitPane JTable JList JTextField DefaultCellEditor JLabel JOptionPane)
   (javax.swing.table AbstractTableModel DefaultTableCellRenderer TableCellEditor TableCellRenderer)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (javax.swing.event TableModelListener TableModelEvent)
   (java.awt BorderLayout Color Font Dimension)
   (java.awt.event ItemListener ItemEvent FocusListener)))


(defn- mk-model [state]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      2)
    (getRowCount []
      ;(await state)
      (int (Math/ceil (/ (count (:clips @state)) 2))))
    (getValueAt [row col]
      ;(await state)
      (:name (nth (:clips @state) (+ col (* 2 row)) {})))
    (isCellEditable [row col]
      false)))

(defn- build-table [state]
  (proxy [JTable] []
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [f (doto (JLabel. value)
                    (.setFont (Font. "Monospaced" Font/PLAIN 16)))
                clip-name (.getValueAt (.getModel table) row col)
                is-active? (sequencer/is-clip-active?
                            clip-name)
                is-selected? (contains? (:selected-clips @state) clip-name)
                is-marked-active? (contains? (:clips-marked-to-be-active @state) clip-name)
                is-marked-inactive? (contains? (:clips-marked-to-be-inactive @state) clip-name)]
            (cond
              (and is-selected? is-active?)
              (doto f
                (.setForeground (Color/BLACK))
                (.setBackground (Color/PINK))
                (.setOpaque true))
              is-marked-active? (doto f
                                  (.setForeground (Color/BLACK))
                                  (.setBackground (Color/ORANGE))
                                  (.setOpaque true))
              is-marked-inactive? (doto f
                                    (.setForeground (Color/BLACK))
                                    (.setBackground (Color/CYAN))
                                    (.setOpaque true))
              is-selected? (doto f
                             (.setForeground (Color/BLACK))
                             (.setBackground (Color/LIGHT_GRAY))
                             (.setOpaque true))
              is-active? (doto f
                           (.setForeground (Color/BLACK))
                           (.setBackground (Color/GREEN))
                           (.setOpaque true)))
            (when hasFocus
              (if is-active?
                (.setBorder f (LineBorder. Color/BLACK 2))
                (.setBorder f (LineBorder. Color/YELLOW 2))))            
            f))))))

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

(defn create [state]
  (let [table-model (mk-model state)
        table (doto (build-table state)
                (.setModel table-model)
                (.setTableHeader nil)
                (.addFocusListener focus-listener))
        _ (add-watch state :clip-table (fn [key state old new]
                                         (.fireTableStructureChanged table-model)))]
    (utils/add-key-action
           table "control D" "delete-clip"
           (let [row (.getSelectedRow table)
                 col (.getSelectedColumn table)
                 clip-name (.getValueAt table-model row col)
                 clip-pos (some #(when (= (:name (second %)) clip-name)
                              (first %))
                           (map-indexed vector (:clips @state)))
                 choice (JOptionPane/showConfirmDialog
                         nil
                         (str "Delete " clip-name "?")
                         "Delete clip"
                         JOptionPane/YES_NO_CANCEL_OPTION)]
             (when (and clip-pos (= choice JOptionPane/YES_OPTION))
               (send state update :clips
                     #(into (subvec % 0 clip-pos) (subvec % (inc clip-pos))))
               (sequencer/rm-clip clip-name))))
    
    (utils/add-key-action
        table "control A" "set-clip-active"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)]
          (if-not (empty? (:selected-clips @state))
            (do
              (doseq [c (:selected-clips @state)]
                (sequencer/set-clip-active c true))
              (send state update :selected-clips empty))
            (sequencer/set-clip-active (.getValueAt table-model row col) true))
          (.fireTableDataChanged table-model)))

    (utils/add-key-action
        table "control alt A" "set-clip-active-and-mute"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)]
          (if-not (empty? (:selected-clips @state))
            (do
              (doseq [c (:selected-clips @state)]
                (s.sc/update-clip-vol c 0 true)
                (sequencer/set-clip-active c true))
              (send state update :selected-clips empty))
            (do
              (s.sc/update-clip-vol (.getValueAt table-model row col) 0 true)
              (sequencer/set-clip-active (.getValueAt table-model row col) true)))
          (.fireTableDataChanged table-model)))

    (utils/add-key-action
        table "shift alt A" "mark-clip-to-be-active"
      (let [row (.getSelectedRow table)
            col (.getSelectedColumn table)]
        (send state update :clips-marked-to-be-active
              #(conj (set %) (.getValueAt table-model row col)))
        (.fireTableDataChanged table-model)))

    (utils/add-key-action
        table "shift alt X" "mark-clip-to-be-inactive"
      (let [row (.getSelectedRow table)
            col (.getSelectedColumn table)]
        (send state update :clips-marked-to-be-inactive
              #(conj (set %) (.getValueAt table-model row col)))
        (.fireTableDataChanged table-model)))

    (utils/add-key-action
        table "shift alt S" "process-marked-clips"
      (doseq [c (:clips-marked-to-be-inactive @state)]
        (sequencer/set-clip-active c false))
      (doseq [c (:clips-marked-to-be-active @state)]
        (sequencer/set-clip-active c true))
      (send state #(-> %
                       (update :clips-marked-to-be-inactive empty)
                       (update :clips-marked-to-be-active empty)))
      (.fireTableDataChanged table-model))
    
    (utils/add-key-action
        table "control shift A" "toggle-select-clip"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)
              clip-name (.getValueAt table-model row col)]
          (send state update :selected-clips
                #(let [s (set %)]
                   (if (contains? s clip-name)
                     (disj s clip-name)
                     (conj s clip-name))))
          (.fireTableDataChanged table-model)))
    
    (utils/add-key-action
        table "control shift X" "unselect-all-clips"
      (send state update :selected-clips empty)
      (.fireTableDataChanged table-model))
    
    (utils/add-key-action
        table "control X" "set-clip-inactive"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)]
          (if-not (empty? (:selected-clips @state))
            (do
              (doseq [c (:selected-clips @state)]
                (sequencer/set-clip-active c false))
              (send state update :selected-clips empty))
            (sequencer/set-clip-active (.getValueAt table-model row col) false))
          (.fireTableDataChanged table-model)))
    
    (utils/add-key-action
        table "shift DOWN" "reduce-vol"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)]
          (if-not (empty? (:selected-clips @state))
            (doseq [c (:selected-clips @state)]
              (s.sc/update-clip-vol c -0.01))
            (s.sc/update-clip-vol (.getValueAt table-model row col) -0.05))))
    
    (utils/add-key-action
        table "shift UP" "increase-vol"
        (let [row (.getSelectedRow table)
              col (.getSelectedColumn table)]
          (if-not (empty? (:selected-clips @state))
            (doseq [c (:selected-clips @state)]
              (s.sc/update-clip-vol c 0.01))
            (s.sc/update-clip-vol (.getValueAt table-model row col) 0.05))))
    (JScrollPane. table)))
