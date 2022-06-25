(ns seqr.tui
  (:require [seqr.scheduler :as sched]
            [seqr.helper :as helper]
            [seqr.clip :as clip]
            [seqr.midi :as midi]
            [seqr.connections :as conn])
  (:import (javax.swing JTextPane JFrame JTextField JScrollPane JLabel JTable JPanel BoxLayout AbstractAction KeyStroke JComponent BorderFactory)
           (javax.swing.text DefaultHighlighter$DefaultHighlightPainter StyleContext$NamedStyle StyleConstants)
           (javax.swing.border LineBorder)
           (javax.swing.table AbstractTableModel TableCellRenderer)
           (java.awt Dimension BorderLayout Font Color FlowLayout)
           (java.awt.event KeyListener KeyEvent ActionEvent)
           (seqr.tui ClipRenderer)))


(defonce ^:private tui-frame (atom nil))

;; :clips {:group-1 {:clip-1 {} :clip-2 {}}}
(defonce ^:private tui-state (atom {:selected [0 0] :clips {} :editing-clip nil :clip-selected {} :group-selected 0}))

(defn find-clip [name]
  (some (fn [[_ m]]
          (if (contains? m name)
            (get m name)))
        (:clips @tui-state)))

(defn clip-table-model [num]
  (proxy [AbstractTableModel] []
    (getRowCount []
      4)
    (getColumnCount []
      4)
    (getValueAt [^Integer row ^Integer column]
      (let [groups (-> @tui-state :clips keys sort)
            group (if (< num (count groups))
                    (nth groups num))
            clips (keys (get (:clips @tui-state) group {}))
            idx (if group
                  (dec (helper/get-point (inc row) (inc column) 4)))
            clip (if (and (number? idx) (< idx (count clips)))
                   (nth clips idx))]
        (if clip
          (str clip)
          "")))
    (getColumnClass [^Integer idx]
      (.getClass String))))

(defn update-clip-data [clip-groups clips]
  (let [groups (:clips @tui-state)]
    (loop [t 0 g (-> groups keys sort)]
      (when (first g)
        (.setText (nth clip-groups t) (-> g first name)))
      (.fireTableDataChanged (.getModel (nth clips t)))
      (if (< t (dec (count clips)))
        (recur (inc t) (rest g))))))


(defn set-editor-content [^JTextPane editor text]
  (let [{positions :clip-positions div :clip-div point :clip-point} @tui-state
        doc (.getStyledDocument editor)
        default (.getStyle editor "editor-default")
        [opt-start opt-end] (get-in positions [1 1] [0 (.length text)])]
    (.setText editor text)
    (.setCharacterAttributes doc opt-start (- opt-end opt-start) default  true)
    (doseq [p (range 1 point)]
      (when-let [[start end] (get-in positions (helper/get-pos p div))]
        (let [doc (.getStyledDocument editor)
              style (.getStyle editor (str "action-" (-> (helper/get-pos p div) second)))]
          (.setCharacterAttributes doc start (- end start) style true))))))

(defn move-selection [grid dir]
  (try
    (let [[row col] (:selected @tui-state [0 0])
          max-col (-> grid count dec)
          new-col (condp = dir
                    "right" (if (< col max-col)
                              (inc col)
                              0)
                    "left" (if (> col 0)
                             (dec col)
                             max-col)
                    col)
          max-row (-> grid (nth new-col) count dec)
          new-row (condp = dir
                    "up" (if (> row 0)
                           (dec row)
                           max-row)
                    "down" (if (< row max-row)
                             (inc row)
                             0)
                    (if (> row max-row)
                      max-row
                      row))
          old (-> grid (nth col) (nth row))
          new (-> grid (nth new-col) (nth new-row))]
      (when (= new-col 1)
        (swap! tui-state assoc :group-selected new-row))
      (swap! tui-state assoc :selected [new-row new-col])
      (.setBorder old (BorderFactory/createEmptyBorder))
      (.setBorder new (LineBorder. Color/YELLOW 2))
      (.repaint new 0 0 0 800 500)
      (.repaint old 0 0 0 800 500)
      (.grabFocus new))
    (catch Exception e
      (prn e))))

(defn move-sub-selection [table idx dir]
  (let [[row col] (get-in @tui-state [:clip-selected idx] [0 0])
        max-row (-> table (.getModel) (.getRowCount))
        max-col (-> table (.getModel) (.getColumnCount))
        new-col (condp = dir
                    "right" (if (< col max-col)
                              (inc col)
                              0)
                    "left" (if (> col 0)
                             (dec col)
                             max-col)
                    col)
        new-row (condp = dir
                    "up" (if (> row 0)
                           (dec row)
                           max-row)
                    "down" (if (< row max-row)
                             (inc row)
                             0)
                    row)]
    (swap! tui-state assoc-in [:clip-selected idx] [new-row new-col])
    (-> table (.getModel) (.fireTableDataChanged))))

(defn open-clip [editor table idx tables]
  (let [[row col] (get-in @tui-state [:clip-selected idx])
        name (-> table (.getModel) (.getValueAt row col))]
    (when name
      (let [key (if (= (.charAt name 0) \:)
                  (keyword (.substring name 1))
                  name)
            {:keys [point div] :as cl} (first
                                        (mapcat (fn [[group clips]]
                                                  (if (and (map? clips)
                                                           (contains? clips key))
                                                    [(get clips key)]
                                                    []))
                                                (:clips @tui-state)))
            [positions text] (clip/as-str (assoc cl :name name))]
        (swap! tui-state merge {:editing-clip name
                                :clip-div div
                                :clip-point point
                                :clip-positions positions})
        (doseq [t tables]
          (.repaint t))
        (set-editor-content editor text)))))


(defn save-clip [^JTextPane editor player-add-clip add?]
  (let [text (.getText editor)
        cl (clip/parse-clip text)
        [positions text] (clip/as-str cl)
        name (:name cl)
        old-groups (map first (filter #(contains? (second %) name) (:clips @tui-state)))
        group (:group cl "default")
        caret-pos (.getCaretPosition editor)]
    (swap! tui-state #(assoc
                        (assoc-in
                         (reduce (fn [m g]
                                   (update-in m [:clips g] dissoc name))
                                 % old-groups)
                         [:clips group name] cl)
                        :clip-div (:div cl)
                        :clip-positions positions
                        :clip-point (:point cl)
                        :editing-clip name))
    (set-editor-content editor text)
    (.setCaretPosition editor caret-pos)
    (when add?
      (player-add-clip name cl))))

(defn rand-color []
  (rand-nth [Color/GREEN Color/ORANGE Color/WHITE
             Color/YELLOW Color/CYAN Color/PINK Color/RED]))


(def rand-colors (repeatedly rand-color))

(defn create-tui [id-ref state-ref toggle-player add-player-clip rm-player-clip]
  (remove-watch id-ref :player-tui-id)
  (remove-watch state-ref :player-tui)
  (when (and @tui-frame (.isValid @tui-frame))
    (.dispose @tui-frame))
  (let [root (JPanel. (BorderLayout.))
        frame (doto (JFrame. "Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))
        editor (doto (JTextPane.)
                 (.setBackground Color/BLACK)
                 (.setForeground Color/GREEN)
                 (.setCaretColor Color/WHITE)
                 (.setEditable true)
                 (.setBorder (LineBorder. Color/YELLOW 3))
                 (.setFont (Font. "Monospaced" Font/PLAIN 14)))
        active-action (doto (.addStyle editor "active-action" nil)
                        (StyleConstants/setBold true)
                        (StyleConstants/setBackground (.brighter Color/YELLOW))
                        (StyleConstants/setForeground Color/BLACK))
        action-styles (doall
                       (map #(doto (.addStyle editor (str "action-" %1) nil)
                               (StyleConstants/setBackground Color/BLACK)
                               (StyleConstants/setForeground %2)
                               (StyleConstants/setBold false))
                            (range 1 33) (take 32 rand-colors)))
        default-style (doto (.addStyle editor "editor-default" nil)
                        (StyleConstants/setBold false)
                        (StyleConstants/setBackground Color/BLACK)
                        (StyleConstants/setForeground Color/GREEN))
        [player-size player-div player-bpm player-point recording] (repeatedly 5 #(doto (JTextField. 4) (.setEditable false)))
        player-bar (doto (JPanel. (FlowLayout. FlowLayout/LEFT 10 10))
                     (.add (JLabel. "Size"))
                     (.add player-size)
                     (.add (JLabel. "Div"))
                     (.add player-div)
                     (.add (JLabel. "BPM"))
                     (.add player-bpm)
                     (.add (JLabel. "Point"))
                     (.add player-point)
                     (.add (JLabel. "Recording?"))
                     (.add recording))
        pane (JScrollPane. editor)
        colors (repeatedly 8 rand-color)
        group-titles (map #(doto (JLabel.)
                             (.setFont (Font. "Monospaced" Font/PLAIN 20))
                             (.setForeground %))
                          colors)
        clips (map #(doto (JTable. (clip-table-model %))
                      (.setShowGrid false)
                      (.setSize 160 100)
                      (.setDefaultRenderer (.getClass String)
                                           (ClipRenderer. tui-state % (nth colors %)))
                      (.setFont (Font. "Monospaced" Font/PLAIN 16))
                      (.setBackground Color/BLACK))
                   (range 0 8))
        clip-table (JPanel.)
        clip-table (doto clip-table
                     (.setBackground Color/BLACK)
                     (.setLayout (BoxLayout. clip-table BoxLayout/Y_AXIS)))
        _ (doseq [i (range 0 (count clips))]
            (.add clip-table (nth group-titles i))
            (.add clip-table (nth clips i)))
        state-changed (fn [state]
                        (try
                          (.setText player-size (str (:size state)))
                          (.setText player-div (str (:div state)))
                          (.setText player-bpm (str (:bpm state)))
                          (let [groups (reduce
                                        (fn [groups [name data]]
                                          (update groups
                                                  (:group data "default")
                                                  assoc name
                                                  (assoc data :in-player true)))
                                        (:clips @tui-state) (:clips state))]
                            (swap! tui-state update :clips merge groups)
                            (swap! tui-state update :in-player (fn [l] (-> state :clips keys)))
                            (update-clip-data group-titles clips))
                          (catch Exception e
                            (prn e))))
        component-grid [[editor] clips]
        move-select (fn [dir]
                      (proxy [AbstractAction] []
                        (actionPerformed [^ActionEvent e]
                          (move-selection component-grid dir))))
        move-sub-select (fn [table idx dir]
                          (proxy [AbstractAction] []
                            (actionPerformed [^ActionEvent e]
                              (move-sub-selection table idx dir))))
        open-selected (fn [table idx]
                        (proxy [AbstractAction] []
                          (actionPerformed [^ActionEvent e]
                            (open-clip editor table idx clips))))
        add-selected (fn [table idx]
                       (proxy [AbstractAction] []
                         (actionPerformed [^ActionEvent e]
                           (let [[row col] (get-in @tui-state [:clip-selected idx])
                                 name (-> table (.getModel) (.getValueAt row col))
                                 cl (find-clip name)]
                             (add-player-clip name cl)
                             (update-clip-data group-titles clips)))))
        rm-selected (fn [table idx]
                      (proxy [AbstractAction] []
                        (actionPerformed [^ActionEvent e]
                          (let [[row col] (get-in @tui-state [:clip-selected idx])
                                name (-> table (.getModel) (.getValueAt row col))]
                            (rm-player-clip name)
                            (update-clip-data group-titles clips)))))
        del-selected (fn [table idx]
                       (proxy [AbstractAction] []
                         (actionPerformed [^ActionEvent e]
                           (let [[row col] (get-in @tui-state [:clip-selected idx])
                                 name (-> table (.getModel) (.getValueAt row col))]
                             (swap! tui-state update :clips
                                    (fn [clips]
                                      (reduce #(update %1 %2 dissoc name)
                                              clips (keys clips))))
                             (update-clip-data group-titles clips)
                             (rm-player-clip name)))))
        start-stop (proxy [AbstractAction] []
                     (actionPerformed [^ActionEvent e]
                       (toggle-player)
                       (swap! tui-state assoc :in-player [])))
        add-new-clip (proxy [AbstractAction] []
                     (actionPerformed [^ActionEvent e]
                       (.setText
                        editor
                        "{:args {} :outs {:sc seqr.sc/s-new} :eval seqr.sc/note :name new}")))]

    (update-clip-data group-titles clips)

    (add-watch state-ref :player-tui
               (fn [key state old new]
                 (state-changed (get new @id-ref))
                 (when-let [counter (get new :point)]
                   (add-watch counter :player-tui-point
                              (fn [k s o n]
                                (.setText player-point (str n)))))))

    (add-watch id-ref :player-tui-id
               (fn [key state old new]
                 (state-changed (get @state-ref new))
                 (when-let [counter (get-in @state-ref [new :point])]
                   (add-watch counter :player-tui-point
                              (fn [k s o n]
                                (try
                                  (let [clip-cells (map #(get-in @tui-state [:clip-cells %])
                                                        (:in-player @tui-state))
                                        _ (doseq [c clip-cells] (.blink c true))
                                        div (:clip-div @tui-state 4)
                                        player-div (get-in @state-ref [new :div] 4)
                                        point (:clip-point @tui-state 2)
                                        point (helper/get-wrapped-point n div (dec point) player-div)
                                        ms-period (:ms-period (sched/get-job-info new))]
                                    (when point
                                      (let [[bar note] (helper/get-pos point div)]
                                          (when-let [[start end] (get-in @tui-state [:clip-positions bar note])]
                                            (doto (.getStyledDocument editor)
                                              (.setCharacterAttributes start (- end start) active-action true))
                                            (sched/schedule-task
                                             #(.setCharacterAttributes (.getStyledDocument editor) start (- end start) (nth action-styles note) true)
                                             ms-period))))

                                    (sched/schedule-task
                                     #(doseq [c clip-cells]
                                        (.blink c false))
                                     ms-period))
                                  (catch Exception e
                                    (prn "Error" e)))
                                (.setText player-point (str n)))))))
    (add-watch midi/midi-buffer
               :midi-handler
               (fn [key state old new]
                 (try
                   (let [{:keys [outs args eval] :as cl} (or (find-clip (:editing-clip @tui-state))
                                                             (clip/parse-clip (.getText editor)))
                         [_ msg] (last new)
                         f (clip/midi-interpreter cl)]
                     (when (and f (-> outs empty? not) msg)
                       (let [action (f msg cl)]
                         (when (contains? action :action)
                             (doseq [[dest out-fn] outs]
                               (conn/send! dest
                                           (-> action
                                               (merge args)
                                               eval
                                               out-fn)))))))
                   (catch Exception e
                     (prn e)))))
    
    (swap! tui-state assoc :selected [0 0])
    (.grabFocus editor)

    (doseq [comp (conj clips editor root clip-table)]
      (doseq [c [JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
                 JComponent/WHEN_FOCUSED
                 JComponent/WHEN_IN_FOCUSED_WINDOW]]
        (doseq [[k name action] [["control alt LEFT" "switch-focus-left" (move-select "left")]
                                 ["control alt RIGHT" "switch-focus-right" (move-select "right")]
                                 ["control alt UP" "switch-focus-up" (move-select "up")]
                                 ["control alt DOWN" "switch-focus-down" (move-select "down")]
                                 ["control alt N" "add-new-clip" add-new-clip]
                                 ["shift ENTER" "start-stop" start-stop]]]
          (-> comp
              (.getInputMap c)
              (.put (KeyStroke/getKeyStroke k) name))
          (-> comp
              (.getActionMap)
              (.put name action)))))
    (doseq [i (range 0 (count clips))]
      (let [cl (nth clips i)]
        (doseq [[k name action] [["W" "move-up" (move-sub-select cl i "up")]
                                 ["A" "move-left" (move-sub-select cl i "left")]
                                 ["S" "move-down" (move-sub-select cl i "down")]
                                 ["D" "move-right" (move-sub-select cl i "right")]
                                 ["ENTER" "open-clip" (open-selected cl i)]
                                 ["control A" "add-selected" (add-selected cl i)]
                                 ["control R" "rm-selected" (rm-selected cl i)]
                                 ["control D" "del-selected" (del-selected cl i)]]]
          (-> cl
              (.getInputMap JComponent/WHEN_FOCUSED)
              (.put (KeyStroke/getKeyStroke k) name))
          (-> cl
              (.getActionMap)
              (.put name action)))))

    (doto (.getInputMap editor JComponent/WHEN_FOCUSED)
      (.put (KeyStroke/getKeyStroke "control S") "save-clip")
      (.put (KeyStroke/getKeyStroke "control A") "add-clip")
      (.put (KeyStroke/getKeyStroke "control R") "toggle-recording")
      (.put (KeyStroke/getKeyStroke "control Y") "paste-recording"))

    (doto (.getActionMap editor)
      (.put "save-clip"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent e]
                (save-clip editor add-player-clip false)
                (update-clip-data group-titles clips))))
      (.put "add-clip"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent e]
                (save-clip editor add-player-clip true)
                (update-clip-data group-titles clips))))
      (.put "toggle-recording"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent e]
                (let [v (not (:recording? @tui-state false))]
                  (midi/toggle-recording v)
                  (swap! tui-state assoc :recording? v)
                  (.setText recording (str v))))))
      (.put "paste-recording"
            (proxy [AbstractAction] []
              (actionPerformed [^ActionEvent e]
                (let [bpm (.getText player-bpm)
                      cl (clip/build-from-midi
                          (if (not (empty? bpm))
                            (Long/parseLong bpm)
                            80)
                          (clip/parse-clip (.getText editor)))]
                  (.setText editor (second (clip/as-str cl)))
                  (save-clip editor add-player-clip false))))))

    (doto pane
      (.setPreferredSize (Dimension. 800 500)))
    (doto root
      (.add player-bar BorderLayout/NORTH)
      (.add pane BorderLayout/WEST)
      (.add clip-table BorderLayout/EAST))
    (doto (.getContentPane frame)
      (.add root))
    (doto frame
      (.pack)
      (.setVisible true))
    (reset! tui-frame frame)))



