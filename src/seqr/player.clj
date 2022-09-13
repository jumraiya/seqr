(ns seqr.player
  (:require [seqr.scheduler :as sched]
            [seqr.clip :as clip]
            [seqr.helper :as helper]
            [seqr.tui :as tui]
            [seqr.connections :as conn]))


(defonce ^:private player (atom nil))

(defonce ^:private player-states (atom {}))

(declare play)

(declare is-running?)

(declare stop-player)

(declare update-dynamic)

(sched/start-sched)

(defn get-player [& {:keys [div bpm size paused? play-once? run? id]
                     :or {div 4 bpm 80 size 4 paused? false play-once? false run? true}
                     :as options}]
  "Create a player instance which is essentially a scheduled job in the pool"
  (let [period (helper/calc-period bpm div)
        state {:div div :bpm bpm :size size :clips {} :buffer {} :point (ref 1) :play-once? play-once? :run? run?}
        player-id (if id
                    (sched/start-job period play id)
                    (sched/start-job period play))]
    (swap! player-states assoc player-id state)
    player-id))

(defn start-player [& {:keys [div bpm size] :or {div 4 bpm 80 size 4} :as options}]
  (when (is-running? @player)
    (stop-player))
  (reset! player (get-player options)))

(defn stop-player
  "Stops a given sequencer"
  ([]
   (stop-player @player))
  ([id]
   (sched/stop-job id)
   (conn/send! :sc-gated (seqr.sc/stop-gated {}))
   (swap! player-states dissoc id)))


(defn set-bpm
  ([bpm]
   (set-bpm @player bpm))
  ([player-id bpm]
   (let [old-period (:ms-period (sched/get-job-info player-id))
         ms-period (long (helper/calc-period bpm (get-in @player-states [player-id :div])))]
     (when (not (= old-period ms-period))
       (sched/set-period player-id ms-period)
       (swap! player-states update player-id assoc :bpm bpm)))))

(defn set-running [val]
  (swap! player-states assoc-in [@player :run?] val))

(defn is-running?
  ([]
   (is-running? @player))
  ([id]
   (sched/is-job-running? id)))

(defn toggle-player [& [args]]
  (if (is-running? @player)
    (stop-player)
    (start-player args)))

(defn add-clip
  ([name clip]
   (add-clip name clip @player))
  ([name {:keys [div args] :as clip} player-id]
   (try
     (let [player-div (helper/lcmv div (get-in @player-states [player-id :div]))
           clip-size (-> clip :point dec)
           new-size (apply max
                           (conj
                            (map #(clip/calc-size (second %) player-div)
                                 (get-in @player-states [player-id :clips]))
                            (clip/calc-size clip player-div)))
           new-buffer (loop [buffer (get-in @player-states [player-id :buffer]) point 1]
                        (let [clip-pos (helper/get-pos point div)
                              actions (get buffer point)
                              clip-actions (get-in clip clip-pos)
                              eval-fn (:eval clip)
                              buffer (assoc
                                      buffer
                                      point
                                      (if (-> clip-actions empty? not)
                                        (assoc actions name
                                               (reduce
                                                (fn [point-actions args]
                                                  (reduce
                                                   (fn [byte-data [dest proc-fn]]
                                                     (update byte-data dest
                                                             conj (proc-fn (eval-fn args))))
                                                   point-actions (:outs clip)))
                                                {} clip-actions))
                                        (dissoc actions name)))]
                          (if (< point clip-size)
                            (recur buffer (inc point))
                            buffer)))
           new-buffer (reduce #(dissoc %1 %2)
                              new-buffer
                              (filter #(> % new-size)
                                      (keys new-buffer)))]

       (swap! player-states
              (fn [states]
                (update states player-id
                        #(merge % {:size new-size
                                   :div player-div
                                   :clips (assoc
                                           (:clips %)
                                           name clip)
                                   :buffer new-buffer}))))
       (set-bpm player-id (get-in @player-states [player-id :bpm])))
     (catch Exception e
       (prn "Error adding clip" e)))))

(defn rm-clip
  ([name]
   (rm-clip name @player))
  ([name player-id]
   (when (is-running? player-id)
     (let [bpm (get-in @player-states [player-id :bpm])]
         (swap!
          player-states
          update player-id
          (fn [{:keys [clips buffer div bpm] :as state}]
            (let [new-clips (dissoc clips name)
                  player-div (apply helper/lcmv (map #(:div (second %)) new-clips))
                  player-size (apply max (map #(clip/calc-size (second %) player-div) new-clips))
                  new-state {:clips new-clips
                             :buffer (reduce (fn [b p]
                                               (update b p dissoc name))
                                             buffer (keys buffer))
                             :div player-div
                             :size player-size}]
              (merge state new-state))))
         (set-bpm player-id (get-in @player-states [player-id :bpm]))))))

(defn play [player-id]
  (try
    (when-let [state (get @player-states player-id)]
      (let [{:keys [point size div play-once? run?]} state]
        (when run?
         (doseq [[clip-name cl] (:clips state)]
           (let [{c-div :div c-point :point} cl
                 pt (helper/get-wrapped-point @point c-div (dec c-point) div)]
             (doseq [[dest messages] (get-in state [:buffer pt clip-name])]
               (doseq [m messages]
                 (conn/send! dest m)))))
          (dosync
           (if (>= @point size)
             (if play-once?
               (stop-player player-id)
               (do
                 (ref-set point 1)
                 (sched/schedule-task #(update-dynamic player-id) 10)))
             (commute point inc))))))
    (catch Exception e
      (prn "Error playing" player-id e))))

(defn update-dynamic [player-id]
  (try
    (when-let [state (get @player-states player-id)]
      (let [player-div (:div state)
            clips (:clips state)]
        (doseq [[clip-name {:keys [point div eval outs dynamic] :as cl}] clips]
          (doseq [p dynamic]
            (let [wrapped (helper/get-wrapped-point p div (dec point) player-div)
                  key [player-id :buffer wrapped clip-name]
                  actions (get-in cl (helper/get-pos p div))
                  new-data (reduce
                            (fn [data [dest proc-fn]]
                              (assoc data dest (map #(proc-fn (eval %)) actions)))
                            {} outs)]
              (swap! player-states assoc-in key new-data))))))
    (catch Exception e
      (prn "Error updating player buffer" player-id e))))

(defn update-player [ks val]
  (swap! player-states assoc-in (concat [@player] ks) true))

(defn ui []
  (tui/create-tui player player-states toggle-player add-clip rm-clip update-player))


