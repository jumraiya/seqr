(ns seqr.sequencer
  (:require
   [seqr.helper :as helper]
   [seqr.connections :as conn]
   [seqr.interpreters :as in]
   [seqr.serializers :as se])
  (:import
   (java.util Arrays)
   (java.util.concurrent.locks LockSupport)
   (java.nio ByteBuffer)
   (javax.sound.midi MidiMessage Receiver)))

(def MAX-CLIPS 10)

(def MAX-CLIP-ACTIONS 1024)

(def MAX-ACTION-LEN 1024)

(defonce buffer
  (make-array Byte/TYPE MAX-CLIPS MAX-CLIP-ACTIONS 2 MAX-ACTION-LEN))

(defonce counter (volatile! 0))

(defonce midi-clock-pulses (volatile! 0))

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

(defonce dynamic-actions-thread nil)

(defonce locked-actions (agent {}))

(def MAX-SENDERS (- (.availableProcessors (Runtime/getRuntime)) 2))

(defonce sender-idx (atom 0))

(defonce sender-threads (agent {}))

(defn- clear-slot [slot]
  (doseq [i (range MAX-CLIP-ACTIONS)]
    (Arrays/fill (aget buffer slot i 0) (byte 0))
    (Arrays/fill (aget buffer slot i 1) (byte 0))))

(defn- serialize-actions [slot point clip]
  (let [[b n] (helper/get-pos point (:div clip))
        variant (cond (= 0 (aget buffer slot (dec point) 0 (dec MAX-ACTION-LEN))) 0
                      (= 0 (aget buffer slot (dec point) 1 (dec MAX-ACTION-LEN))) 1
                      :else nil)]
    (when variant
        (when-let [actions (get-in clip [b n])]
          (with-local-vars [f-len [(byte 0) (byte 0)]]
            (loop [offset 0 actions actions]
              (let [[action & actions] actions
                    interpreted (in/interpret clip action)
                    actions (if (sequential? interpreted)
                              (into (vec (rest interpreted)) actions)
                              actions)
                    bytes (se/serialize clip (if (sequential? interpreted)
                                               (first interpreted)
                                               interpreted))
                    len (alength bytes)
                    next-offset (+ offset len 3)
                    len-seq (helper/short->bytes len)
                    bytes (into (if (= offset 0)
                                  (do
                                    (var-set f-len len-seq)
                                    [(byte 0) (byte 0)])
                                  len-seq)
                                bytes)]
                (if (<= next-offset MAX-ACTION-LEN)
                  (do
                    (doseq [i (range (count bytes))]
                      (aset buffer slot (dec point) variant (+ offset i) (nth bytes i)))
                    (if (seq actions)
                      (recur next-offset actions)
                      true))
                  (println "cannot fit actions"))))
            (aset buffer slot (dec point) variant 0 (first @f-len))
            (aset buffer slot (dec point) variant 1 (second @f-len))
            (aset buffer slot (dec point) variant (dec MAX-ACTION-LEN) (byte 1)))))))

(defn- do-send [num local-counter counter buf]
  (.set local-counter @counter)
  (let [read-len (fn [slot point variant a b]
                   (-> buf
                       (.get)
                       (.put 0 (aget buffer slot point variant a))
                       (.put 1 (aget buffer slot point variant b))
                       (.getShort 0)))]
      (doseq [[slot div size dest clip] (get-in @sender-threads [num :clips])]
        (let [rel-div (/ (:div @state) div)
              c (dec (.get local-counter))]
          (when (and (contains? (:active-slots @state) slot)
                     (= 0 (mod c rel-div))
                     (>= c 0))
            (let [point (/ c rel-div)
                  point (if (>= point size)
                          (mod point size)
                          point)
                  variant (cond (> (aget buffer slot point 0 (dec MAX-ACTION-LEN)) 0) 0
                                (> (aget buffer slot point 1 (dec MAX-ACTION-LEN)) 0) 1
                                :else nil)]
              (when variant
                  (loop [offset 0]
                    (let [len (read-len slot point variant offset (inc offset))]
                      (when (> len 0)
                        (conn/send! dest (aget buffer slot point variant)
                                    (int (+ offset 2)) (int len)))
                      (when (and (< (+ offset len 3) MAX-ACTION-LEN)
                                 (> len 0))
                        (recur (+ offset len 3)))))
                  (when (and (contains? (:dynamic clip) (inc point))
                             (> (aget buffer slot point
                                      (if (= variant 0) 1 0)
                                      (dec MAX-ACTION-LEN)) 0))
                    (aset buffer slot point variant (dec MAX-ACTION-LEN) (byte 0))))))))))


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
        (catch Exception e
          (prn e)
          (println (str "Terminating " (.getName this))))))))

(defn- mk-dynamic-actions-thread []
  (proxy [Thread] ["dynamic-actions-thread"]
    (run []
      (while (not (:terminate? @state))
        (if (:running? @state)
          (try
            (doseq [[slot div size _ clip] (mapcat :clips (vals @sender-threads))]
              (when (contains? (:active-slots @state) slot)
                (doseq [p (:dynamic clip)]
                  (serialize-actions slot p clip))))
            (catch Exception _))
          (LockSupport/park this))))))

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
          (swap! sender-idx (constantly next-idx)))))))

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

(defn get-div []
  (:div @state 4))

(defn get-size
  ([]
   (:size @state))
  ([state]
   (reduce (fn [s [_ div point]]
             (max s (* point
                       (/ (:div state) div))))
           0
           (filter #(contains? (:active-slots state) (first %))
                   (mapcat :clips (vals @sender-threads))))))

(defn set-play-window [start end]
  (send state assoc :start-counter start :end-counter end))

(defn reset-play-window []
  (send state dissoc :start-counter :end-counter))

(defn get-play-window []
  (when (contains? @state :start-counter)
   [(:start-counter @state) (:end-counter @state)]))

(defn get-active-clip-names []
  (into #{}
        (comp
         (map :clips)
         (mapcat #(into [] (comp
                            (filter
                             (fn [[slot]]
                               (contains? (:active-slots @state) slot)))
                            (map last)
                            (map :name))
                        %)))
        (vals @sender-threads)))

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
                                                     (:div new-state))))))))))))

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

(defn midi-clock-receiver []
  (proxy [Receiver] []
    (close []
      )
    (send [^MidiMessage m _]
      (try
                                        ;(prn (.getStatus m) (.getCommand m) (.getData1 m) (.getData2 m))
        (condp = (.getStatus m)
          250 (do
                (vreset! counter (or (:start-counter @state) 1))
                (vreset! midi-clock-pulses 0))
          252 (do
                (vreset! counter (or (:start-counter @state) 1))
                (vreset! midi-clock-pulses 0))
          248 (if (= 23 @midi-clock-pulses)
                (do
                  (vreset! midi-clock-pulses 0)
                  (vswap! counter inc))
                (vswap! midi-clock-pulses inc))
          nil)
        (catch Exception e
          (prn e))))))

(defn use-midi-clock [status]
  (send state assoc :use-midi-clock status))

(defn using-midi-clock? []
  (:use-midi-clock @state false))

(defn- mk-counter-thread []
  (proxy [Thread] []
    (run []
      (while (not (:terminate? @state))
        (try
          (if (and (:running? @state)
                   (not (using-midi-clock?)))
            (do
              (if (< @counter (or (:end-counter @state) (:size @state)))
                (vswap! counter inc)
                (vreset! counter (or (:start-counter @state) 1)))
              (Thread/sleep (:period @state)))
            (do
              (println "Stopping sequencer")
              (LockSupport/park this)))
          (catch Exception e
                                        ;(prn "Exception in counter" e)
            ))))))

(defn start []
  (vreset! counter (or (:start-counter @state) 0))
  (send state assoc :running? true :terminate? false)
  (await-for 100 state)
  (when (or (nil? counter-thread)
            (= (.getState ^Thread counter-thread)
               Thread$State/TERMINATED))
    (alter-var-root (var counter-thread) (constantly (mk-counter-thread))))
  (when (or (nil? dynamic-actions-thread)
            (= (.getState ^Thread dynamic-actions-thread)
               Thread$State/TERMINATED))
    (alter-var-root (var dynamic-actions-thread) (constantly (mk-dynamic-actions-thread))))
  (let [resume #(condp = (.getState ^Thread %)
                  Thread$State/NEW (.start ^Thread %)
                  Thread$State/RUNNABLE (println (str (.getName %) " already running"))
                  Thread$State/WAITING (LockSupport/unpark %)
                  Thread$State/TIMED_WAITING (println (str (.getName %) " is timed waiting"))
                  Thread$State/BLOCKED (println (str (.getName %) " is blocked"))
                  Thread$State/TERMINATED :terminated)]
    (resume counter-thread)
    (resume dynamic-actions-thread)
    (doseq [[idx {t :thread}] @sender-threads]
      (let [make-new? (or (nil? t)
                          (when t
                            (= (resume t) :terminated)))]
        (when make-new?
          (send sender-threads
                assoc-in [idx :thread] (doto (sender-thread idx counter)
                                         (.start))))))
    (doseq [l (vals (get @callbacks sequencer-started))]
      (l))))

(defn pause []
  (send state assoc :running? false)
  (send state (fn [s]
                (let [div (or (apply helper/lcmv
                                     (into [] (comp (map :clips)
                                                    (mapcat #(map last %))
                                                    (map :div))
                                           (vals @sender-threads))) 4)]
                  (assoc s :div div :period (long (/ 60000 (:bpm s 80) div))))))
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
  (when dynamic-actions-thread
    (.interrupt dynamic-actions-thread))
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
  (swap! sender-idx (constantly 0))
  (send sender-threads (constantly {})))

(defn reset-state []
  (doseq [i (range MAX-CLIPS)]
    (clear-slot i))
  (if (agent-error state)
    (restart-agent state default-state)
    (send state (constantly default-state)))
  (swap! sender-idx (constantly 0))
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
    (println (str "counter state: " (.getState counter-thread))))
  (when dynamic-actions-thread
    (println (str "dynamic actions state: " (.getState dynamic-actions-thread))))
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
