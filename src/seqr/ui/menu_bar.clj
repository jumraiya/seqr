(ns seqr.ui.menu-bar
  (:require
   [seqr.clip :as clip]
   [seqr.ui.utils :as utils])
  (:import
   (javax.swing JMenuBar JMenu JMenuItem JFileChooser)))


(defn load-sketch [path editor state]
  (try
    (let [{:keys [clips]} (read-string (slurp path))]
      (send state assoc :clips (mapv clip/parse-clip clips)))
    (catch Exception e
      (prn "Error loading sketch" e))))

(defn save-sketch [path state]
  (try
    (let [clips (reduce
                 #(conj %1 (second (clip/as-str %2)))
                  [] (:clips @state))
          sketch {:clips clips}]
      (spit path (pr-str sketch)))
    (catch Exception e
      (prn "Error saving sketch" e))))

(defn- save-load-sketch [editor state load?]
  (let [chooser (JFileChooser.)
        res (if load?
              (.showOpenDialog chooser editor)
              (.showSaveDialog chooser editor))]
    (when (= res JFileChooser/APPROVE_OPTION)
      (let [path (->> chooser
                      (.getSelectedFile)
                      (.getAbsolutePath))]
        (if load?
          (load-sketch path editor state)
          (save-sketch path state))))))

(defn build [state]
  (let [open (JMenuItem. "Open")
        save-as (JMenuItem. "Save As")
        file-menu (doto (JMenu. "File")
                    (.add open)
                    (.add save-as))]
    (utils/add-action-listener
     open
     (save-load-sketch (.getTopLevelAncestor open) state true))
    (utils/add-action-listener
        save-as
      (save-load-sketch (.getTopLevelAncestor open) state false))
    (doto (JMenuBar.)
      (.add file-menu))))