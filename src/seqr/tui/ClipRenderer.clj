(ns seqr.tui.ClipRenderer
  (:import (javax.swing JTextArea JFrame JTextField JScrollPane JLabel JTable JPanel BoxLayout AbstractAction KeyStroke JComponent BorderFactory)
           (javax.swing.border LineBorder)
           (javax.swing.table AbstractTableModel TableCellRenderer)
           (java.awt Dimension BorderLayout Font Color Rectangle)
           (java.awt.event KeyListener KeyEvent ActionEvent)))

(defn clip-setStyle [this]
  (let [{:keys [editing-clip in-player] :or {in-player []}} @(:tui-state (.state this))
        val (or (.getText this) "")
        in-player? (contains? (set in-player) val)]
    (cond
      (= val editing-clip) (doto this
                             (.setOpaque true)
                             (.setBackground Color/YELLOW)
                             (.setForeground Color/BLACK))
      in-player? (doto this
                   (.setOpaque true)
                   (.setBackground Color/GREEN)
                   (.setForeground Color/BLACK))
      true (doto this
             (.setOpaque false)
             (.setBackground Color/BLACK)
             (.setForeground (:color (.state this)))))))

(defn clip-getTableCellRendererComponent [this ^JTable tbl ^Object val ^Boolean selected? ^Boolean focused? ^Integer row ^Integer col]
  (try
    (let [{:keys [group-selected clip-selected clips] :or {group-selected 0} :as tui} @(:tui-state (.state this))
          [r c] (get clip-selected group-selected [0 0])
          key (when (-> val empty? not)
                (if (= (.charAt val 0) \:)
                  (keyword (.substring val 1))
                  val))
          idx (-> this (.state) :idx)]
      (if (and (= group-selected idx)
               (and (= r row) (= c col)))
        (.setBorder this (LineBorder. Color/WHITE 1))
        (.setBorder this (BorderFactory/createEmptyBorder))))
    (.setText this (str val))
    (clip-setStyle this)
    (swap! (:tui-state (.state this)) assoc-in [:clip-cells val] this)
    (catch Exception e
      (prn e)))
  this)

(defn clip-blink [this on?]
  (if on?
    (doto this
      (.setOpaque true)
      (.setBackground Color/WHITE)
      (.setForeground Color/BLACK))
    (clip-setStyle this)))

(defn clip-init [^clojure.lang.Atom tui-state ^Long idx ^Color color]
  [[] {:tui-state tui-state :idx idx :color color}])

(gen-class
 :name seqr.tui.ClipRenderer
 :implements [javax.swing.table.TableCellRenderer]
 :extends javax.swing.JLabel
 :constructors {[clojure.lang.Atom Long java.awt.Color] []}
 :methods [[blink [Boolean] void]]
 :init "init"
 :state "state"
 :prefix "clip-")
