(ns seqr.ui.clip-config
  (:require [clojure.string :as str]
            [seqr.ui.utils :as utils])
  (:import
   (java.awt.event ComponentListener ComponentEvent)
   (javax.swing JComboBox JTextPane JScrollPane JSplitPane JTable JList JTextField DefaultCellEditor JLabel)
   (javax.swing.table AbstractTableModel DefaultTableCellRenderer TableCellEditor TableCellRenderer)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (javax.swing.event TableModelListener TableModelEvent)
   (java.awt BorderLayout Color Font Dimension)
   (java.awt.event ItemListener ItemEvent FocusListener)))

(def CELL-MAX-WIDTH 20)

(def CELL-MIN-WIDTH 5)

(def FONT-SIZE 15)

(def ^:private default-properties
  [:name :dest :interpreter :div])

(def ^:private default-props-set
  (set default-properties))

#_(defn- set-column-sizes [clip-atom table]
    (let [grid-map (build-grid-map @clip-atom (.getWidth table))]
      (doseq [c (range (.getColumnCount table))]
        (when-let [w (get grid-map [:max-len c])]
          (.setMinWidth (.getColumn (.getColumnModel table) c) w))))
    (.doLayout table))

(defn- set-table-height [scroll-pane table]
  (let [view-size (Dimension. (.getWidth (.getViewSize (.getViewport scroll-pane)))
                              (* (.getRowHeight table) (.getRowCount table)))]
    (.setViewSize (.getViewport scroll-pane) view-size)))

(defn- config-model [clip-atom table]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      8)
    (getRowCount []
      (let [args (-> @clip-atom :data :args keys sort)]
        (inc (int (Math/ceil (/ (* 2 (count args)) 8))))))
    (getValueAt [row col]
      (let [args (flatten
                  (into (mapv #(vector % (get-in @clip-atom [:data %])) default-properties)
                        (->> @clip-atom :data :args (sort-by key))))
            val (nth args (+ (* row 8) col) nil)]
        (if (keyword? val)
          (name val)
          (str val))))
    (setValueAt [val row col]
      (let [args (flatten
                  (into (mapv #(vector % (get-in @clip-atom [:data %])) default-properties)
                        (->> @clip-atom :data :args (sort-by key))))
            prop (nth args (+ (* row 8) (dec col)) nil)]
        (when prop
          (send clip-atom assoc-in
                (if (contains? (set default-properties) prop)
                  [:data prop]
                  [:data :args (name prop)])
                (cond
                  (re-matches #"\d+" val) (Integer/parseInt val)
                  (re-matches #"[\d\.]+" val) (Float/parseFloat val)
                  :else val)))))
    (isCellEditable [row col]
      (odd? col))))

#_(defn- watch [clip container table]
  (add-watch clip :config-table
             (fn [key state old new]
               (prn new)
               (.fireTableStructureChanged (.getModel table))
               )))

#_(defn- on-container-resize [clip table]
  (reify ComponentListener
    (componentHidden [this e])
    (componentMoved [this e])
    (componentResized [this e]
      (.fireTableStructureChanged (.getModel table))
      (set-column-sizes clip table))
    (componentShown [this e])))

(defn- mk-list-listener [prop clip-atom table]
  (reify ItemListener
    (itemStateChanged [this e]
      (when (= (.getStateChange e) ItemEvent/SELECTED)
        (send clip-atom assoc-in [:data (keyword prop)] (.getItem e))
        (.fireTableDataChanged (.getModel table))))))

(defn- mk-list-selector [prop options clip-atom table]
  (DefaultCellEditor.
   (doto (JComboBox. (into-array options))
     (.addItemListener (mk-list-listener prop clip-atom table))
     )))

(defn- build-config-table [clip-atom destinations interpreters]
  (proxy [JTable] []
    (getCellEditor
      ([]
       (proxy-super getCellEditor))
      ([row col]
       (condp = [row col]
         [0 3] (mk-list-selector "dest" destinations clip-atom this)
         [0 5] (mk-list-selector "interpreter" interpreters clip-atom this)
         (proxy-super getCellEditor row col))))
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [f (JLabel. value)]
            (when hasFocus
              (.setBorder f (LineBorder. Color/YELLOW 2)))            
            f))))))

(defn build-table [clip-atom ^JSplitPane container-pane destinations interpreters]
  (let [table (build-config-table clip-atom destinations interpreters)
        model (config-model clip-atom table)
        ;_ (watch clip-atom container-pane table)
        ;_ (.addComponentListener container-pane (on-container-resize clip-atom table))
        table (doto table
                (.setTableHeader nil)
                (.setModel model)
                (.setFont (Font. "Monospaced" Font/PLAIN FONT-SIZE))
                (.setGridColor Color/WHITE))
        _ (utils/add-key-action
           table "control D" "delete-prop"
           (let [r (.getSelectedRow table)
                 c (.getSelectedColumn table)
                 prop (if (odd? c)
                        (.getValueAt model r (dec c))
                        (.getValueAt model r c))]
             (when (and prop r (> r 0))
               (send clip-atom update-in [:data :args] dissoc prop)
               (.fireTableStructureChanged model))))
        scroll-pane (JScrollPane. table)
        _ (.addTableModelListener
           model
           (reify TableModelListener
             (tableChanged [this e]
               (set-table-height scroll-pane table))))]
    (set-table-height scroll-pane table)
    scroll-pane))

