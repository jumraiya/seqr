(ns seqr.ui.clip-table
  (:require
   [seqr.ui.utils :as utils]
   [seqr.sequencer :as sequencer])
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

(defn- build-table []
  (proxy [JTable] []
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [f (doto (JLabel. value)
                    (.setFont (Font. "Monospaced" Font/PLAIN 16)))]
            (when (sequencer/is-clip-active?
                   (.getValueAt (.getModel table) row col))
              (doto f
                (.setForeground (Color/BLACK))
                (.setBackground (Color/GREEN))
                (.setOpaque true)))
            (when hasFocus
              (.setBorder f (LineBorder. Color/YELLOW 2)))            
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
        table (doto (build-table)
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
                     #(into (subvec % 0 clip-pos) (subvec % (inc clip-pos)))))))
    (utils/add-key-action
        table "control A" "set-clip-active"
      (let [row (.getSelectedRow table)
            col (.getSelectedColumn table)]
        (sequencer/set-clip-active (.getValueAt table-model row col) true)
        (.fireTableDataChanged table-model)))
    (utils/add-key-action
        table "control X" "set-clip-inactive"
      (let [row (.getSelectedRow table)
            col (.getSelectedColumn table)]
          (sequencer/set-clip-active (.getValueAt table-model row col) false)
          (.fireTableDataChanged table-model)))
    (JScrollPane. table)))
