(ns seqr.ui.clip-config
  (:require [clojure.string :as str])
  (:import
   (java.awt.event ComponentListener ComponentEvent)
   (javax.swing JComboBox JTextPane JScrollPane JSplitPane JTable JList JTextField DefaultCellEditor)
   (javax.swing.table AbstractTableModel DefaultTableCellRenderer TableCellEditor)
   (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
   (javax.swing.border LineBorder)
   (javax.swing.event TableModelListener TableModelEvent)
   (java.awt BorderLayout Color Font Dimension)
   (java.awt.event ItemListener ItemEvent)))

(def CELL-MAX-WIDTH 20)

(def CELL-MIN-WIDTH 5)

(def FONT-SIZE 15)

(def ^:private default-properties
  [:dest :interpreter :div])

(defonce grid-map (atom {}))

(defn- build-grid-map [clip container-width]
  (loop [grid-map {}
         row 0 col 0
         props (into (mapv #(vector % (-> clip :data %)) default-properties)
                     (into (sorted-map) (-> clip :data :args)))]
    (let [[prop val] (first props)
          prop-width (max CELL-MIN-WIDTH (* FONT-SIZE (-> prop name count)))
          val-width  (min CELL-MAX-WIDTH (max CELL-MIN-WIDTH (* FONT-SIZE (-> val str count))))
          row-has-props? (some? (get grid-map [row 0]))
          row        (if (and (< container-width (+ prop-width val-width))
                              row-has-props?)
                       (inc row)
                       row)
          props (rest props)
          grid-map (-> grid-map
                       (assoc [row col] (name prop))
                       (assoc [row (inc col)] (-> val str))
                       (assoc [:max-len col] (max (get grid-map [:max-len col] 0)
                                                  prop-width))
                       (assoc [:max-len (inc col)] (max (get grid-map [:max-len (inc col)] 0)
                                                        val-width))
                       (assoc :num-rows (inc row)))]
      (if (not (empty? props))
        (recur grid-map row (+ col 2) props)
        grid-map))))


(defn- config-model [clip-atom]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      (inc
       (apply max
              (->> @grid-map keys (filter vector?) (mapv second) (filter number?)))))
    (getRowCount []
      (:num-rows @grid-map))
    (getValueAt [row col]
      (get @grid-map [row col]))
    (setValueAt [val row col]
      (let [prop (keyword (get @grid-map [row (dec col)]))]
        (swap! grid-map assoc [row col] val)
        (swap! clip-atom assoc-in
               (if (contains? (set default-properties) prop)
                 [:data prop]
                 [:data :args prop])
               val)))
    (isCellEditable [row col]
      (odd? col))))

(defn- watch [clip container table]
  (add-watch clip :config-table
             (fn [key state old new]
               (reset! grid-map
                       (build-grid-map new (.getWidth container)))
               )))

(defn- on-container-resize [clip table config-model]
  (reify ComponentListener
    (componentHidden [this e])
    (componentMoved [this e])
    (componentResized [this e]
      (reset! grid-map
              (build-grid-map @clip (.getWidth table)))
      (.fireTableStructureChanged config-model))
    (componentShown [this e])))

(defn- mk-list-listener [prop clip-atom config-model]
  (reify ItemListener
    (itemStateChanged [this e]
      (when (= (.getStateChange e) ItemEvent/SELECTED)
        (let [[[r c]] (first (filter #(= (second %) prop) @grid-map))]
          (swap! grid-map assoc [r (inc c)] (.getItem e))
          (swap! clip-atom assoc-in [:data (keyword prop)] (.getItem e))
          (.fireTableDataChanged config-model))))))

(defn- mk-list-selector [prop options clip-atom config-model]
  (DefaultCellEditor.
   (doto (JComboBox. (into-array options))
     (.addItemListener (mk-list-listener prop clip-atom config-model)))))

(defn- build-config-table [clip-atom destinations interpreters config-model]
  (proxy [JTable] []
    (getCellEditor
      ([]
       (proxy-super getCellEditor))
      ([row col]
       (condp = (get @grid-map [row (dec col)])
         "dest" (mk-list-selector "dest" destinations clip-atom config-model)
         "interpreter" (mk-list-selector "interpreter" interpreters clip-atom config-model)
         (proxy-super getCellEditor row col)))
      )))

(defn build-table [clip-atom ^JSplitPane container-pane destinations interpreters]
  (let [_ (reset! grid-map (build-grid-map @clip-atom (.getWidth container-pane)))
        model (config-model clip-atom)
        table (build-config-table clip-atom destinations interpreters model)
        _ (watch clip-atom container-pane table)
        _ (.addComponentListener container-pane (on-container-resize clip-atom table model))
        table (doto table
                (.setTableHeader nil)
                (.setModel model)
                (.setFont (Font. "Monospaced" Font/PLAIN FONT-SIZE)))]
    (JScrollPane. table)))

