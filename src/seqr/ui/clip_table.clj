(ns seqr.ui.clip-table
  (:import
   (java.awt.event ComponentListener ComponentEvent)
   (javax.swing JComboBox JTextPane JScrollPane JSplitPane JTable JList JTextField DefaultCellEditor JLabel)
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
          (let [f (JLabel. value)]
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
                (.addFocusListener focus-listener)
                (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        _ (add-watch state :clip-table (fn [key state old new]
                                         (.fireTableStructureChanged table-model)))]
    (JScrollPane. table)))
