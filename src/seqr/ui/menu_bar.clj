(ns seqr.ui.menu-bar
  (:require
   [seqr.clip :as clip]
   [seqr.ui.utils :as utils]
   [seqr.ui.editor :as editor]
   [seqr.sequencer :as sequencer])
  (:import
   (javax.swing JMenuBar JMenu JMenuItem JFileChooser)))


(defn load-sketch [path editor state ui-reset-fn]
  (try
    (let [{:keys [bpm clips]} (read-string (slurp path))]
      (editor/reset-state)
      (sequencer/reset-state)
      (ui-reset-fn)
      (doseq [c (mapv clip/parse-clip clips)]
        (editor/save-clip state c))
      (sequencer/set-bpm (or bpm 80)))
    (catch Exception e
      (prn "Error loading sketch" e))))

(defn save-sketch [path state ui-reset-fn]
  (try
    (let [clips (reduce
                 #(conj %1 (second (clip/as-str %2)))
                  [] (:clips @state))
          sketch {:clips clips :bpm (sequencer/get-bpm)}]
      (spit path (pr-str sketch)))
    (catch Exception e
      (prn "Error saving sketch" e))))

(defn- save-load-sketch [editor state load? ui-reset-fn]
  (let [chooser (JFileChooser.)
        res (if load?
              (.showOpenDialog chooser editor)
              (.showSaveDialog chooser editor))]
    (when (= res JFileChooser/APPROVE_OPTION)
      (let [path (->> chooser
                      (.getSelectedFile)
                      (.getAbsolutePath))]
        (if load?
          (load-sketch path editor state ui-reset-fn)
          (save-sketch path state ui-reset-fn))))))

(defn- new-sketch [state]
  (editor/reset-state)
  (sequencer/reset-state))

(defn build [state ui-reset-fn]
  (let [new (JMenuItem. "New")
        open (JMenuItem. "Open")
        save-as (JMenuItem. "Save As")
        file-menu (doto (JMenu. "File")
                    (.add new)
                    (.add open)
                    (.add save-as))]
    (utils/add-action-listener
        new
      (ui-reset-fn)
      (new-sketch state))
    (utils/add-action-listener
        open
        (save-load-sketch (.getTopLevelAncestor open) state true ui-reset-fn))
    (utils/add-action-listener
        save-as
        (save-load-sketch (.getTopLevelAncestor open) state false ui-reset-fn))
    (doto (JMenuBar.)
      (.add file-menu))))
