(ns seqr.ui.tracker
  (:require
   [seqr.sequencer :as sequencer]
   [seqr.helper :as helper]
   [seqr.clip :as clip]
   [seqr.ui.utils :as utils])
  (:import
   (javax.swing JTable JLabel)
   (javax.swing.border LineBorder)
   (javax.swing.table AbstractTableModel TableCellRenderer)
   (java.awt Color Font)
   (java.awt.event FocusListener)))

(defonce active-row (atom nil))

(def ODD-COLOR (Color. 85 85 85))

(def EVEN-COLOR (Color. 83 104 120))

(defn- get-clip [state col]
  (let [clips (filter #(sequencer/is-clip-active? (:name %)) (:clips @state))]
    (when (and (> col 0) (<= col (count clips)))
      (nth clips (dec col)))))

(defn- get-actions-str [state row col]
  (when-let [{:keys [div point] :as cl} (get-clip state col)]
    (when-let [seq-div (sequencer/get-div)]
      (let [point (helper/get-wrapped-point (inc row) div (dec point) seq-div)
            pos (when point
                  (helper/get-pos point div))
            actions (when pos
                      (get-in cl pos))]
        (when (seq actions)
          (clip/mk-action-str actions cl))))))

(defn- mk-model [state save-clip-fn table]
  (proxy [AbstractTableModel] []
    (getColumnCount []
      (inc (count (sequencer/get-active-clip-names))))
    (getRowCount []
      (sequencer/get-size))
    (getValueAt [row col]
      (cond
        (= col 0) (inc row)
        :else (get-actions-str state row col)))
    (setValueAt [val row col]
      (when-let [{:keys [div point] :as cl} (get-clip state col)]
        (let [actions (get-in (clip/parse-clip val cl) [1 1])
              at-point (helper/get-wrapped-point
                        (inc row) div (dec point) (sequencer/get-div))
              new-clip (assoc-in cl (helper/get-pos at-point div) actions)]
          (save-clip-fn state (if (and (seq actions)
                                       (< (:point new-clip 1) (inc at-point)))
                                (assoc new-clip :point (inc at-point))
                                new-clip)))))
    (isCellEditable [row col]
      (int?
       (when-let [{:keys [div point] :as cl} (get (:clips @state) (dec col))]
         (when-let [seq-div (sequencer/get-div)]
           (helper/get-wrapped-point (inc row) div (dec point) seq-div)))))
    (fireTableStructureChanged []
      (let [max-row (.getMaxSelectionIndex (.getSelectionModel table))
            min-row (.getMinSelectionIndex (.getSelectionModel table))
            selected-col (.getSelectedColumn table)
            row-count (sequencer/get-size)
            col-count (inc (count (sequencer/get-active-clip-names)))]
          (proxy-super fireTableStructureChanged)
          (when (and (< max-row row-count) (>= min-row 0) (>= selected-col 0) (< selected-col col-count))
            (.setRowSelectionInterval table min-row max-row)
            (.setColumnSelectionInterval table  selected-col selected-col))))))


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

(defn- build-table [state set-clip-fn]
  (proxy [JTable] []
    (getCellRenderer [row col]
      (reify TableCellRenderer
        (getTableCellRendererComponent [this table value isSelected hasFocus row col]
          (let [{:keys [div point]} (get-clip state col)
                f (doto (JLabel. ^String (str value))
                    (.setFont (Font. "Monospaced" Font/PLAIN 16)))]

            (when (and div (> col 0) (= 0 (mod row (/ (sequencer/get-div) div))))
              (let [pos (/ row (/ (sequencer/get-div) div))]
                  (doto f
                    (.setBackground (if (or (<= (inc pos) (dec point))
                                            (even? (quot pos (dec point))))
                                      EVEN-COLOR
                                      ODD-COLOR))
                    (.setOpaque true))))
            
            (when (contains? (set (.getSelectedRows table)) row)
              (doto f
                (.setBackground Color/WHITE)
                (.setForeground Color/BLACK)
                (.setBorder (LineBorder. Color/BLACK 1))
                (.setOpaque true)))
            (when-let [[start end] (sequencer/get-play-window)]
              (when
               (and (>= (inc row) start) (<= (inc row) end))
                (doto f
                  (.setBackground Color/GREEN)
                  (.setForeground Color/BLACK)
                  (.setBorder (LineBorder. Color/BLACK 1))
                  (.setOpaque true))))
            (when (= row @active-row)
              (doto f
                (.setBackground (.brighter Color/YELLOW))
                (.setForeground Color/BLACK)
                (.setOpaque true)))
            (when hasFocus
              (.setBorder f (LineBorder. Color/RED 2))
              (set-clip-fn (get-clip state col)))
            f))))))

(defn- open-editor [state table e]
  (let [r (.getSelectedRow table)
        c (.getSelectedColumn table)
        val (.getValueAt (.getModel table) r c)
        clip (get-clip state c)]
    (when (and (:div clip) (:point clip))
      (let [seq-div (or (sequencer/get-div) (:div clip))]
        (utils/show-action-editor
         (.getTopLevelAncestor (.getSource e))
         clip
         (helper/get-wrapped-point (inc r) (:div clip) (dec (:point clip)) seq-div)
         #(.setValueAt (.getModel table) % r c))))))

(defn build [state set-clip-fn save-clip-fn]
  (let [table (build-table state set-clip-fn)
        model (mk-model state save-clip-fn table)
        table (doto table
                (.setModel model)
                (.setTableHeader nil)
                (.addFocusListener focus-listener)
                (.setCellSelectionEnabled true)
                (.setRowSelectionAllowed true)
                (.setSelectionBackground Color/WHITE)
                (.setSelectionForeground Color/BLACK))]
    (utils/add-key-action
        table "control E" "edit-action"
        (open-editor state table e))
    
    (sequencer/register-callback sequencer/clip-made-active :refresh-tracker
                                 (fn [_]
                                   (.fireTableStructureChanged model)))
    (sequencer/register-callback sequencer/clip-made-inactive :refresh-tracker
                                 (fn [_]
                                   (.fireTableStructureChanged model)))
    table))
