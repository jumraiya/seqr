(ns seqr.ui.utils
  (:require
   [clojure.pprint :refer [pprint]]
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.interpreters :as inter]
   [seqr.serializers :as ser])
  (:import
   (javax.swing AbstractAction JButton JDialog JTabbedPane KeyStroke JComponent JTextPane)
   (java.awt Color Font Dimension)
   (javax.swing.event ChangeListener)
   (java.awt.event ActionListener)))

(defmacro add-key-action-with-focus [component key action focus & body]
  (let [ev (gensym)
        body (helper/replace-syms {'e ev} body)]
    `(do
       (doto (.getInputMap ~component ~focus)
         (.put (KeyStroke/getKeyStroke ~key) ~action))
       (doto (.getActionMap ~component)
         (.put ~action
               (proxy [AbstractAction] []
                 (actionPerformed [~ev]
                   ~@body)))))))

(defmacro add-key-action [component key action & body]
  `(add-key-action-with-focus ~component ~key ~action JComponent/WHEN_FOCUSED ~@body))


(defmacro add-action-listener [component & body]
  (let [ev (gensym)
        body (helper/replace-syms {'e ev} body)]
    `(.addActionListener
      ~component
      (reify ActionListener
        (actionPerformed [this ~ev]
          ~@body)))))

(defmacro add-change-listener [component & body]
  (let [ev (gensym)
        body (helper/replace-syms {'e ev} body)]
    `(.addChangeListener
      ~component
      (reify ChangeListener
        (stateChanged [this ~ev]
          ~@body)))))

(defn text-pane []
  (doto (JTextPane.)
    (.setBackground Color/BLACK)
    (.setForeground Color/GREEN)
    (.setCaretColor Color/WHITE)
    (.setEditable true)
    (.setFont (Font. "Monospaced" Font/PLAIN 14))))

(defn- mk-dialog [source msg]
  (let [dialog (JDialog. source msg true)]
    (doto dialog
      (.setLocation (.getLocation source))
      (.setSize (Dimension. 400 300))
      (.setDefaultCloseOperation JDialog/DISPOSE_ON_CLOSE))))

(defn show-text-input-dialog [source msg value callback]
  (let [input (doto (text-pane)
                (.setText value))
        dialog (mk-dialog source msg)]
    (add-key-action input "control S" "commit"
                    (callback (.getText input))
                    (.dispose dialog))
    (doto dialog
      (.add input)
      (.setVisible true))))

(defn show-action-editor [source clip point callback]
  (let [pos (helper/get-pos point (:div clip))
        actions (get-in clip pos)
        action-str (clip/mk-action-str actions clip)
        pane-1 (doto (text-pane)
                 (.setText action-str))
        pane-2 (doto (text-pane)
                 (.setEditable false))
        pane-3 (doto (text-pane)
                 (.setEditable false))
        pane (doto (JTabbedPane. )
               (.addTab "Value" pane-1)
               (.addTab "Interpreted" pane-2)
               (.addTab "Serialized" pane-3))
        dialog (mk-dialog source "Edit Action")]
    (add-change-listener
        pane
        (cond
          (= 1 (.getSelectedIndex pane))
          (let [actions (clip/parse-actions (.getText pane-1) clip)]
            (.setText pane-2 (with-out-str
                               (pprint (mapv #(inter/interpret clip %) actions)))))
          (= 2 (.getSelectedIndex pane))
          (let [actions (clip/parse-actions (.getText pane-1) clip)]
            (.setText pane-3 (with-out-str
                               (pprint (mapv #(->> %
                                                   (inter/interpret clip)
                                                   (ser/serialize clip)
                                                   byte-array
                                                   (String.))
                                             actions)))))
          :else nil))
    (doseq [c [pane-1 pane-2]]
      (add-key-action c "control S" "commit"
        (callback (.getText pane-1))
        (.dispose dialog)))
    (doto dialog
      (.add pane)
      (.setVisible true))))
