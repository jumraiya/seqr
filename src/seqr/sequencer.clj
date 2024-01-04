(ns seqr.sequencer
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.connections :as conn]
   [seqr.interpreters :as in]
   [seqr.serializers :as se])
  (:import
   (java.util Arrays)
   (java.util.concurrent.locks LockSupport)
   (java.nio ByteBuffer)))

(def MAX-CLIPS 10)

(def MAX-CLIP-ACTIONS 256)

(def MAX-ACTION-LEN 1024)

(defonce buffer
  (make-array Byte/TYPE MAX-CLIPS MAX-CLIP-ACTIONS MAX-ACTION-LEN))

(defonce counter (volatile! 0))

(defonce ^:private callbacks (atom {}))

(def clip-saved ::clip-saved)

(def clip-deleted ::clip-deleted)

(def clip-made-active ::clip-made-active)

(def clip-made-inactive ::clip-made-inactive)

(def sequencer-started ::seqr-started)

(def sequencer-paused ::seqr-paused)

(def events #{clip-saved clip-deleted clip-made-active clip-made-inactive sequencer-started sequencer-paused})

(def ^:private default-state
  {:period 93
   :div 4
   :size 4
   :bpm 80
   :active-slots #{}
   :clip-slots {}
   :running? false
   :terminate? false
   :available-slots (set (range MAX-CLIPS))})

(defonce state (agent default-state))

(defonce counter-thread nil)

(def MAX-SENDERS (- (.availableProcessors (Runtime/getRuntime)) 2))

(defonce sender-idx (agent 0))

(defonce sender-threads (agent {}))

(defn- clear-slot [slot]
  (doseq [i (range MAX-CLIP-ACTIONS)]
    (Arrays/fill (aget buffer slot i) (byte 0))))

(defn- serialize-actions [slot point clip]
  (let [[b n] (helper/get-pos point (:div clip))]
    (when-let [actions (get-in clip [b n])]
      (loop [offset 0 actions actions]
        (let [[action & actions] actions
              bytes (->> action (in/interpret clip) (se/serialize clip))
              len (alength bytes)
              next-offset (+ offset len 3)
              ;; a (unchecked-byte len)
              ;; b (-> len (unsigned-bit-shift-right 8) unchecked-byte)
              ;;bytes (into [a b] bytes)
              bytes (into (helper/short->bytes len) bytes)]
          (if (<= next-offset MAX-ACTION-LEN)
            (do
              (doseq [i (range (count bytes))]
                (aset buffer slot (dec point) (+ offset i) (nth bytes i)))
              (if (seq actions)
                (recur next-offset actions)
                true))
            (println "cannot fit actions")))))))

 (defn- do-send [num local-counter counter buf]
   (.set local-counter @counter)
   (doseq [[slot div size dest clip] (get-in @sender-threads [num :clips])]
     (let [rel-div (/ (:div @state) div)
           c (dec (.get local-counter))]
       (when (and (contains? (:active-slots @state) slot)
                  (= 0 (mod c rel-div))
                  (>= c 0))
         (loop [offset 0]
           (let [point (/ c rel-div)
                 point (if (>= point size)
                         (mod point size)
                         point)
                 ;; len (-> (short 0)
                 ;;         (bit-or (aget buffer slot point (inc offset)))
                 ;;         (bit-shift-left 8)
                 ;;         (bit-or (aget buffer slot point offset)))
                 len (-> buf
                         (.get)
                         (.put 0 (aget buffer slot point offset))
                         (.put 1 (aget buffer slot point (inc offset)))
                         (.getShort 0))]
             (when (> len 0)
               (conn/send! dest (aget buffer slot point)
                           (int (+ offset 2)) (int len))
               (when (contains? (:dynamic clip) (inc point))
                   (future
                     (serialize-actions slot (inc point) clip))))
             (when (and (< (+ offset len 3) MAX-ACTION-LEN)
                        (> len 0))
               (recur (+ offset len 3)))))))))


(defn sender-thread [num counter]
  (proxy [Thread] [^String (str "sender-" num)]
    (run []
      (try
        (let [local-counter (doto (ThreadLocal.)
                              (.set 0))
              buf (doto (ThreadLocal.)
                    (.set (ByteBuffer/allocateDirect 2)))]
            (while (not (:terminate? @state))
              (if (:running? @state)
                (if (= (.get local-counter) @counter)
                  (Thread/onSpinWait)
                  (do-send num local-counter counter buf))
                (LockSupport/park this))))
        (doseq [[slot] (get-in @sender-threads [num :clips])]
          (send state update :active-slots disj slot))
                                        ;(send sender-threads dissoc num)
        (catch Exception e
          (prn e)
          (println (str "Terminating " (.getName this))))))))

(defn- assign-sender-clip [slot {:keys [div point dest] :as clip}]
  (when (and div point dest)
    (let [[sender pos] (some (fn [[num {:keys [clips]}]]
                               (some (fn [[pos [clip-slot]]]
                                       (when (= slot clip-slot)
                                         [num pos]))
                                     (map-indexed vector clips)))
                             @sender-threads)
          clip-size (dec point)]
      (if sender
        (send sender-threads update-in [sender :clips] assoc pos [slot div clip-size dest clip])
        (let [next-idx  (if (< @sender-idx MAX-SENDERS)
                          (inc @sender-idx)
                          1)]
          (if (contains? @sender-threads next-idx)
            (send sender-threads update-in [next-idx :clips] conj [slot div clip-size dest clip])
            (send sender-threads
                  assoc next-idx {:thread (cond-> (sender-thread next-idx counter)
                                            (:running? @state) (.start))
                                  :clips [[slot div clip-size dest clip]]}))
          (send sender-idx (constantly next-idx)))))))

(defn- save-clip-to-buffer [slot {:keys [interpreter serializer div point dest] :as clip}]
  (clear-slot slot)
  (when (and div point)
    (doseq [p (range 1 point)]
      (serialize-actions slot p clip))))

(defn set-bpm [bpm]
  (send state #(-> %
                   (assoc :bpm bpm)
                   (assoc :period (long (/ 60000 bpm (:div %)))))))

(defn get-bpm []
  (:bpm @state 80))

(defn- get-size [state]
  (reduce (fn [s [_ div point]]
            (max s (* point
                      (/ (:div state) div))))
          0
          (filter #(contains? (:active-slots state) (first %))
                  (mapcat :clips (vals @sender-threads)))))

(defn save-clip [{:keys [name div point dest] :as clip} & [play?]]
  (when (and name div point dest)
    (when-let [slot (or (get-in @state [:clip-slots (:name clip)])
                        (first (:available-slots @state)))]
      (send state #(-> %
                       (update :available-slots disj slot)
                       (update :clip-slots assoc (:name clip) slot)))
      (let [clip (reduce (fn [cl f]
                           (f cl)) 
                         clip
                         (vals (get @callbacks clip-saved)))]
        (save-clip-to-buffer slot clip)
        (assign-sender-clip slot clip)
        (when (or play? (contains? (:active-slots @state) slot))
          (send state update :active-slots disj slot)
                                        ;(await state)
          (send state #(let [new-state (-> %
                                           (update :div helper/lcmv (:div clip))
                                           (update :active-slots conj slot))]
                         (-> new-state
                             (assoc :size (get-size new-state))
                             (assoc :period (long (/ 60000 (:bpm new-state)
                                                     (:div new-state)))))))))
      )))

(defn set-clip-active [name active?]
  (when-let [slot (get-in @state [:clip-slots name])]
    (when-let [[num pos div point] (some (fn [[num {:keys [clips]}]]
                                           (some (fn [[pos [clip-slot div point]]]
                                                   (when (= slot clip-slot)
                                                     [num pos div point]))
                                                 (map-indexed vector clips)))
                                         @sender-threads)]
      (send state update :active-slots disj slot)
      (when active?
        (await state)
        (send state
              #(let [new-state (-> %
                                   (update :div helper/lcmv div)
                                   (update :active-slots conj slot))]
                 (->  new-state
                      (assoc :size (get-size new-state))
                      (assoc :period (long (/ 60000 (:bpm new-state) (:div new-state)))))))
        (doseq [f (vals (get @callbacks clip-made-active))]
          (f (get-in @sender-threads [num :clips pos]))))
      (when-not active?
        (send state assoc :size (get-size @state))
        (doseq [f (vals (get @callbacks clip-made-inactive))]
          (f (get-in @sender-threads [num :clips pos])))))))

(defn rm-clip [name]
  (when-let [slot (get-in @state [:clip-slots name])]
    (when-let [clip (some (fn [[num {:keys [clips]}]]
                            (some (fn [[_ [clip-slot _ _ _ clip]]]
                                    (when (= slot clip-slot)
                                      clip))
                                  (map-indexed vector clips)))
                          @sender-threads)]
        (clear-slot slot)
        (send state #(-> %
                         (update :clip-slots dissoc name)
                         (update :available-slots conj slot)
                         (update :active-slots disj slot)))
        (doseq [f (vals (get @callbacks clip-deleted))]
          (f clip)))))

(defn- mk-counter-thread []
  (proxy [Thread] []
    (run []
      (while (not (:terminate? @state))
        (try
          (if (:running? @state)
            (do
              (if (< @counter (:size @state))
                (vswap! counter inc)
                (vreset! counter 1))
              (Thread/sleep (:period @state)))
            (do
              (println "Stopping sequencer")
              (LockSupport/park this)))
          (catch Exception e
            ;(prn "Exception in counter" e)
            ))))))

(defn start []
  (vreset! counter 0)
  (send state assoc :running? true :terminate? false)
  (await-for 100 state)
  (when (or (nil? counter-thread)
            (= (.getState ^Thread counter-thread)
               Thread$State/TERMINATED))
    (alter-var-root (var counter-thread) (constantly (mk-counter-thread))))
  (let [resume #(condp = (.getState ^Thread %) 
                 Thread$State/NEW (.start ^Thread %)
                 Thread$State/RUNNABLE (println (str (.getName %) " already running"))
                 Thread$State/WAITING (LockSupport/unpark %)
                 Thread$State/TIMED_WAITING (println (str (.getName %) " is timed waiting"))
                 Thread$State/BLOCKED (println (str (.getName %) " is blocked"))
                 Thread$State/TERMINATED :terminated)]
    (resume counter-thread)
    (doseq [[idx {t :thread}] @sender-threads]
      (when t
        (let [r (resume t)]
          (when (= r :terminated)
            (send sender-threads
                  assoc-in [idx :thread] (doto (sender-thread idx counter)
                                           (.start)))))))
    (doseq [l (vals (get @callbacks sequencer-started))]
      (l))))

(defn pause []
  (send state assoc :running? false)
  (doseq [l (vals (get @callbacks sequencer-paused))]
    (l)))

(defn start|pause []
  (if (:running? @state)
    (pause)
    (start))
  (await state))

(defn is-running? []
  (:running? @state))

(defn stop []
  (when-not (agent-error state)
    (send state assoc :terminate? true :running? false)
    (await state))
  (when counter-thread
    (.interrupt counter-thread))
  (doseq [t (mapv :thread (vals @sender-threads))]
    (when t
      (.interrupt t)))
  (send sender-threads (constantly {})))


(defn reset-all []
  (stop)
  (doseq [i (range MAX-CLIPS)]
    (clear-slot i))
  (if (agent-error state)
    (restart-agent state default-state)
    (send state (constantly default-state)))
  (send sender-idx (constantly 0))
  (send sender-threads (constantly {})))

(defn reset-state []
  (doseq [i (range MAX-CLIPS)]
    (clear-slot i))
  (if (agent-error state)
    (restart-agent state default-state)
    (send state (constantly default-state)))
  (send sender-idx (constantly 0))
  (doseq [{:keys [clips]} (vals @sender-threads)]
    (doseq [[_ _ _ _ cl] clips]
      (doseq [f (vals (get @callbacks clip-deleted))]
          (f cl))))
  (send sender-threads update-vals #(assoc % :clips []))
  (await-for 2000 state sender-threads))

(defn clear-data []
  (doseq [i (range MAX-CLIPS)]
    (clear-slot i)))

(defn print-state []
  (prn @state)
  (when counter-thread
    (println (str "counter state " (.getState counter-thread))))
  (println "senders")
  (doseq [[_ {t :thread cls :clips}] @sender-threads]
    (when t
      (println (str (.getName t) " state: " (.getState t)))
      (println (str "sender clips: " (mapv butlast cls))))))

(defn is-clip-active? [name]
  (contains? (:active-slots @state)
             (get (:clip-slots @state) name)))

(defn register-callback [event key f]
  (swap! callbacks assoc-in [event key] f))

(defn reprocess-clips []
  (doseq [{:keys [clips]} (vals @sender-threads)]
    (doseq [data clips]
      (save-clip (last data)))))
