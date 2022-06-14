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


(sched/start-sched)

(defn get-player [& {:keys [div bpm size paused? id] :or {div 4 bpm 80 size 4 paused? false} :as options}]
  "Create a player instance which is essentially a scheduled job in the pool"
  (let [period (helper/calc-period bpm div)
        state {:div div :bpm bpm :size size :clips {} :buffer {} :point (ref 1)}
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
   (swap! player-states dissoc id)))


(defn set-paused [{:keys [player pause?] :or {player @player pause? true}}]
  (swap! player {:pause? pause?}))

(defn is-running?
  ([]
   (is-running? @player))
  ([id]
   (sched/is-job-running? id)))

(defn toggle-player []
  (if (is-running? @player)
    (stop-player)
    (start-player)))

(defn add-clip
  ([name clip]
   (add-clip name clip @player))
  ([name {:keys [div args] :as clip} player-id]
   (try
     (let [player-div (helper/lcmv div (get-in @player-states [player-id :div]))
           clip-size (clip/calc-size clip player-div)
           new-size (apply max
                           (conj
                            (map #(clip/calc-size (second %) player-div)
                                 (get-in @player-states [player-id :clips]))
                            clip-size))
           new-buffer (loop [buffer (get-in @player-states [player-id :buffer]) point 1]
                        (let [clip-pos (helper/get-pos point div :player-div player-div)
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
                                   :buffer new-buffer})))))
     (catch Exception e
       (prn "Error adding clip" e)))))

(defn rm-clip
  ([name]
   (rm-clip name @player))
  ([name player-id]
   (when (is-running? player-id)
       (swap!
        player-states
        update player-id
        (fn [{:keys [clips buffer] :as state}]
          (let [new-clips (dissoc clips name)
                player-div (apply helper/lcmv (map #(:div (second %)) new-clips))
                player-size (apply max (map #(clip/calc-size (second %) player-div) new-clips))
                new-state {:clips new-clips
                           :buffer (reduce (fn [b p]
                                             (update b p dissoc name))
                                           buffer (keys buffer))
                           :div player-div
                           :size player-size}]
            (merge state new-state)))))))

(defn play [player-id]
  (try
    (when-let [state (get @player-states player-id)]
      (let [{:keys [point size div]} state]
        (doseq [[clip-name cl] (:clips state)]
          (let [{c-div :div c-point :point} cl
                pt (helper/get-wrapped-point @point c-div (dec c-point) div)]
            (doseq [[dest messages] (get-in state [:buffer pt clip-name])]
              (doseq [m messages]
                (conn/send! dest m)))))
        (dosync
         (if (>= @point size)
           (ref-set point 1)
           (commute point inc)))))
    (catch Exception e
      (prn "Error playing" player-id e))))

(defn ui []
  (tui/create-tui player player-states toggle-player add-clip rm-clip))
