(ns seqr.ui.utils
  (:require
   [seqr.helper :as helper])
  (:import
   (javax.swing AbstractAction JDialog KeyStroke JComponent JTextPane)
   (java.awt Color Font Dimension)
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

(defn text-pane []
  (doto (JTextPane.)
    (.setBackground Color/BLACK)
    (.setForeground Color/GREEN)
    (.setCaretColor Color/WHITE)
    (.setEditable true)
    (.setFont (Font. "Monospaced" Font/PLAIN 14))))

(defn show-text-input-dialog [source msg value callback]
  (let [input (text-pane)
        dialog (JDialog. source msg true)]
    (add-key-action input "control S" "commit"
                          (callback (.getText input))
                          (.dispose dialog))
    (.setText input value)
    (doto dialog
      (.setLocation (.getLocation source))
      (.setSize (Dimension. 400 300))
      (.setDefaultCloseOperation JDialog/DISPOSE_ON_CLOSE)
      (.add input)
      (.setVisible true))))
