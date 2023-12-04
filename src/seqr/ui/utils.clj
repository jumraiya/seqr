(ns seqr.ui.utils
  (:require
   [seqr.helper :as helper])
  (:import
   (javax.swing AbstractAction KeyStroke JComponent)))

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
