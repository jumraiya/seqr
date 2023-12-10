(ns seqr.sequencer
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper]
   [seqr.connections :as conn]
   [seqr.interpreters :as in]
   [seqr.serializers :as se])
  (:import
   (java.util Arrays)
   (java.util.concurrent.locks LockSupport)))

(def MAX-CLIPS 10)

(def MAX-CLIP-ACTIONS 256)

(def MAX-ACTION-LEN 1024)

(defonce buffer
  (make-array Byte/TYPE MAX-CLIPS MAX-CLIP-ACTIONS MAX-ACTION-LEN))

(defonce counter (volatile! 0))

(def ^:private default-state
  {:period 1000
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

(defn- do-send [num local-counter counter]
  (vreset! local-counter @counter)
  (doseq [[slot div size dest] (get-in @sender-threads [num :clips])]
    (let [rel-div (/ (:div @state) div)
          c (dec @local-counter)]
      (when (and (contains? (:active-slots @state) slot)
                 (= 0 (mod c rel-div)))
        (loop [offset 0]
          (let [point (/ c rel-div)
                point (if (>= point size)
                        (max (mod point size) size)
                        point)
                len (-> (short 0)
                        (bit-or (aget buffer slot point (inc offset)))
                        (bit-shift-left 8)
                        (bit-or (aget buffer slot point offset)))]
            (when (> len 0)
              (conn/send! dest (aget buffer slot point) (+ offset 2) len))
            (if (and (< (+ offset len 3) MAX-ACTION-LEN)
                     (> len 0))
              (recur (+ offset len 3))
              nil)))
        ))))

(defn sender-thread [num counter]
  (let [local-counter (volatile! 0)]
    (proxy [Thread] [^String (str "sender-" num)]
      (run []
        (try
          (while (not (:terminate? @state))
            (if (:running? @state)
              (if (= @local-counter @counter)
                (Thread/onSpinWait)
                (do-send num local-counter counter ))
              (LockSupport/park this)))
          (doseq [[slot] (get-in @sender-threads [num :clips])]
            (send state update :active-slots disj slot))
          ;(send sender-threads dissoc num)
          (catch Exception e
            (prn e)
            (println (str "Terminating " (.getName this)))))))))

(defn- assign-sender-clip [slot {:keys [div point dest]}]
  (let [[sender pos] (some (fn [[num {:keys [clips]}]]
                             (some (fn [[pos [clip-slot]]]
                                     (when (= slot clip-slot)
                                       [num pos]))
                                   (map-indexed vector clips)))
                           @sender-threads)]
    (if sender
      (send sender-threads update-in [sender :clips] assoc pos [slot div point dest])
      (let [next-idx  (if (< @sender-idx MAX-SENDERS)
                        (inc @sender-idx)
                        1)]
        (if (contains? @sender-threads next-idx)
          (send sender-threads update-in [next-idx :clips] conj [slot div point dest])
          (send sender-threads
                assoc next-idx {:thread (cond-> (sender-thread next-idx counter)
                                          (:running? @state) (.start))
                                :clips [[slot div point dest]]}))
        (send sender-idx (constantly next-idx))))))

(defn- save-clip-to-buffer [slot {:keys [div point] :as clip}]
  (clear-slot slot)
  (doseq [p (range 1 point)]
    (when-let [actions (seq (get-in clip (helper/get-pos p div)))]
      (loop [offset 0 actions actions]
        (let [[action & actions] actions
              bytes (->> action (in/interpret clip) (se/serialize clip))
              len (alength bytes)
              next-offset (+ offset len 3)
              a (unchecked-byte len)
              b (-> len (unsigned-bit-shift-right 8) unchecked-byte)
              bytes (into [a b] bytes)]
          (if (<= next-offset MAX-ACTION-LEN)
            (do
              (doseq [i (range (count bytes))]
                (aset buffer slot (dec p) (+ offset i) (nth bytes i)))
              (if (seq actions)
                (recur next-offset actions)
                true))
            (println "cannot fit actions")))))))

(defn save-clip [clip & [play?]]
  (when-let [slot (or (get-in @state [:clip-slots (:name clip)])
                      (first (:available-slots @state)))]
    (send state #(-> %
                     (update :available-slots disj slot)
                     (update :clip-slots assoc (:name clip) slot)))
    (save-clip-to-buffer slot clip)
    (assign-sender-clip slot clip)
    (when (or play? (contains? (:active-slots @state) slot))
      (send state update :active-slots disj slot)
      (await state)
      (send state #(let [new-state (update % :div helper/lcmv (:div clip))]
                     (-> new-state
                         (update :size
                                 max (dec (* (:point clip)
                                             (/ (:div new-state) (:div clip)))))
                         (assoc :period (long (/ 60000 (:bpm new-state) (:div new-state)))))))
      (send state update :active-slots conj slot))))

(defn set-clip-active [name active?]
  (when-let [slot (get-in @state [:clip-slots name])]
    (when-let [[div point] (some (fn [[_num {:keys [clips]}]]
                                   (some (fn [[_pos [clip-slot div point]]]
                                           (when (= slot clip-slot)
                                             [div point]))
                                         (map-indexed vector clips)))
                                 @sender-threads)]
      (send state update :active-slots disj slot)
      (await state)
      (send state
            #(let [new-state (update % :div helper/lcmv div)]
               (update new-state :size max
                       (dec (* point
                               (/ (:div new-state) div))))))
      (send state update :active-slots conj slot))))

(defn rm-clip [name]
  (when-let [slot (get-in @state [:clip-slots name])]
    (clear-slot slot)
    (send state #(-> %
                     (update :clip-slots dissoc name)
                     (update :available-slots conj slot)
                     (update :active-slots disj slot)))))

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
              ;(prn @counter)
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
                 Thread$State/TERMINATED (println (str (.getName %) " is terminated")))]
    (doseq [t (into [counter-thread] (mapv :thread (vals @sender-threads)))]
        (resume t))))

(defn pause []
  (send state assoc :running? false))

(defn stop []
  (send state assoc :terminate? true :running? false)
  (await state)
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
      (println (str "sender clips: " cls)))))