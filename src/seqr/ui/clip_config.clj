(ns seqr.ui.clip-config
  (:require [clojure.string :as str])
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

(defn- build-grid-map [clip container-width]
  (loop [grid-map {}
         row 0 col 0
         props (into (mapv #(vector % (-> clip :data %)) default-properties)
                     (into (sorted-map) (-> clip :data :args)))
         width 0]
    (let [[prop val] (first props)
          prop-width (max CELL-MIN-WIDTH (* FONT-SIZE (-> prop name count)))
          val-width  (min CELL-MAX-WIDTH (max CELL-MIN-WIDTH (* FONT-SIZE (-> val str count))))
          row-has-props? (some? (get grid-map [row 0]))
          new-width      (+ width prop-width val-width)
          row        (if (and (< container-width new-width)
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
        (recur grid-map row (+ col 2) props new-width)
        grid-map))))

(defn- set-column-sizes [clip-atom table]
  (let [grid-map (build-grid-map @clip-atom (.getWidth table))]
      (doseq [c (range (.getColumnCount table))]
        (when-let [w (get grid-map [:max-len c])]
          (.setMinWidth (.getColumn (.getColumnModel table) c) w))))
  (.doLayout table))

(defn- config-model [clip-atom table]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      8
      #_(let [grid-map (build-grid-map @clip-atom (.getWidth table))]
          (inc
           (apply max
                  (->> grid-map keys (filter vector?) (mapv second) (filter number?))))))
    (getRowCount []
      (let [args (-> @clip-atom :data :args keys sort)]
        (inc (int (Math/ceil (/ (* 2 (count args)) 8)))))
      #_(let [grid-map (build-grid-map @clip-atom (.getWidth table))]
          (:num-rows grid-map)))
    (getValueAt [row col]
      (let [args (flatten
                  (into (mapv #(vector % (get-in @clip-atom [:data %])) default-properties)
                        (->> @clip-atom :data :args (sort-by key))))
            val (nth args (+ (* row 8) col) nil)]
        (if (keyword? val)
          (name val)
          (str val)))
      #_(let [grid-map (build-grid-map @clip-atom (.getWidth table))]
          (get grid-map [row col])))
    (setValueAt [val row col]
      (let [args (flatten
                  (into (mapv #(vector % (get-in @clip-atom [:data %])) default-properties)
                        (->> @clip-atom :data :args (sort-by key))))
            prop (nth args (+ (* row 8) (dec col)) nil)]
        (when prop
          (swap! clip-atom assoc-in
                 (if (contains? (set default-properties) prop)
                   [:data prop]
                   [:data :args (name prop)])
                 (cond
                   (re-matches #"\d+" val) (Integer/parseInt val)
                   (re-matches #"[\d\.]+" val) (Float/parseFloat val)
                   :else val))))
      
      #_(let [grid-map (build-grid-map @clip-atom (.getWidth table))
              prop (keyword (get grid-map [row (dec col)]))]
          (swap! clip-atom assoc-in
                 (if (contains? (set default-properties) prop)
                   [:data prop]
                   [:data :args (name prop)])
                 (if (re-matches #"[\d\.]+" val)
                   (Float/parseFloat val)
                   val))))
    (isCellEditable [row col]
      (odd? col))))

(defn- watch [clip container table]
  (add-watch clip :config-table
             (fn [key state old new]
               ;; (.fireTableStructureChanged (.getModel table))
               )))

(defn- on-container-resize [clip table]
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
        (swap! clip-atom assoc-in [:data (keyword prop)] (.getItem e))
        (.fireTableDataChanged (.getModel table))
        #_(let [grid-map (build-grid-map @clip-atom (.getWidth table))
                [[r c]] (first (filter #(= (second %) prop) grid-map))]
            (swap! clip-atom assoc-in [:data (keyword prop)] (.getItem e))
            (.fireTableDataChanged (.getModel table)))))))

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
         (proxy-super getCellEditor row col))
      #_(condp = (get @grid-map [row (dec col)])
         "dest" (mk-list-selector "dest" destinations clip-atom this)
         "interpreter" (mk-list-selector "interpreter" interpreters clip-atom this)
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
                ;(.setShowGrid true)
                ;(.setAutoResizeMode JTable/AUTO_RESIZE_OFF)
                (.setModel model)
                (.setFont (Font. "Monospaced" Font/PLAIN FONT-SIZE))
                (.setGridColor Color/WHITE))
        _ (set-column-sizes clip-atom table)]
    (JScrollPane. table)
    #_(doto (JScrollPane. table)
      (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_NEVER))))

